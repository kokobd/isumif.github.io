---
title: 保护开源项目中的敏感数据
---

## 需求
我们的一个项目中由于包含一段机密字符串，所以无法开源。现在希望在不公开这段字符串的前提下，
开源到GitHub，享受到`Travis CI`和`AppVeyor`对于开源项目提供的免费持续集成服务。

具体来说，代码中存在这样一个常量
```haskell
defIV :: B.ByteString
defIV = B.pack [237,33,6,126,214,53,112,125,148,25,71,73,255,156,253,107]
```
我们需要保证生产环境下使用的`defIV`值不被泄漏

## 解决方案

### 编译阶段确定defIV

为了方便开发，项目中存放一个开发用的`defIV`值，这个值要和生产环境的`defIV`不同。\

我们在cabal包描述中增加一个flag，用来区分环境，针对开发和生产编译出不同的二进制文件
```haskell
flag production
  default: False
```
用stack构建项目时，添加参数`--flag our-package-name:production`
就可以将production设为True了

然后，通过C预处理器把production传递给代码（这段放在`library`里面）
```haskell
if flag(production)
  cpp-options:       -DPRODUCTION
```

代码中使用`configurator`库来读取配置文件。利用C预处理器来决定加载哪一个配置文件
```haskell
#ifdef PRODUCTION
  "config/cipher-production.cfg"
#else
  "config/cipher.cfg"
#endif
```

两个配置文件中定义了同一个属性`iv`，但值不一样。\
开发环境：`iv = "[237,33,6,126,214,53,112,125,148,25,71,73,255,156,253,107]"`\
生产环境：`iv = "$(IV)"\`

也就是说，编译生产环境用的程序时，我们会从环境变量`IV`中读取`iv`的值

**这些配置文件必须在编译阶段读取**，然后将内容注入代码。我们利用TemplateHaskell可以轻松实现
```haskell
readDefIV :: Q Exp
readDefIV = do
  iv :: [Word8] <- runIO $ do
    cfg <- load [Required cipherCfgPath]
    read <$> require cfg "iv"
  return $ ListE $ fmap (LitE . IntegerL . fromIntegral) iv
```
原先的`defIV`，现在就变成了对`readDefIV`的拼接
```haskell
defIV :: B.ByteString
defIV = B.pack $(readDefIV)
```

### production build

我们目前使用的持续集成服务是`Travis CI`和`AppVeyor`，分别针对linux和windows。
它们都支持加密一段敏感数据，具体使用方法参看它们的文档。

这里以`Travis CI`为例。（我还没配`AppVeyor`）
```yaml
env:
  global:
  - secure: # 一些密文，太长了所以略去
```
按照`Travis CI`文档里的说明，我们在`.travis.yml`加入了这样几行。\
`Travis CI` 在构建我们的项目时，会解密这段密文，设置环境变量`IV`。\
只有`Travis CI`才能解密这段密文，所以即使项目开源了，也没有人能看到这段密文对应的明文。

再利用 `Travis CI` 提供的自动部署服务，我们可以把编译好的app自动部署到私有服务器上。
