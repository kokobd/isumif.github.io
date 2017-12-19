---
layout: post
title: Servant Auth (Chinese)
---

## 使用 BasicAuth
BasicAuth是一种基本的http身份验证方法，每次请求时都发送用户名和密码，应当配合https使用。
如同`servant`的其他组件的使用，我们需要在API中描述BasicAuth，然后实现一个Server

`Servant.API.BasicAuth`中包含了两个数据类型: `BasicAuth realm userData`与`BasicAuthData`。这个模块不用单独导入，只要导入`Servant.API`即可

首先自定义你的用户类型，例如
```haskell
data User = User
  { userName :: Text
  , userPwd :: Text
  } deriving (Eq, Show)
```
在某个API中类似这样描述身份验证：
```haskell
type PrivateAPI = "private" :> BasicAuth "some-realm" User :> Get '[JSON] Integer
```
`Integer`可以换成任何你想要返回的数据类型。访问`/private`时需要进行身份验证。

**注意**：如果要保护`/private`的子路经，需要在对应API中添加`BasicAuth`，子路经默认不会被保护。  
例如下面这个API，我们可以不经身份验证访问`/private/public`
```haskell
type BasicAPI = "private" :> "public" :> PublicAPI
  :<|> "private" :> BasicAuth "foo-realm" User :> PrivateAPI
```
需要保护多个子API时，一种做法是利用含参的类型别名，例如
```haskell
type PrivateAPIBase api = "private" :> BasicAuth "/private" User :> api
```

现在来实现满足`PrivateAPI`的Server
```haskell
privateServer :: Server PrivateAPI
privateServer user = pure (10 :: Integer)
```
这里的`Server PrivateAPI`的展开类型为`User -> Handler Integer`，应当是符合预期的。

到这里还不够，我们还需要给`servant`提供身份验证的逻辑  
导入`Servant.Server`模块中的`BasicAuthCheck`，以及`Servant.API`模块中的`BasicAuthData`，我们来写这样一个身份校验函数
```haskell
authCheck :: BasicAuthCheck User
authCheck = BasicAuthCheck $ \(BasicAuthData name_ pwd_) ->
  undefined -- write your authentication logic
```
`BasicAuthCheck user`是对函数类型`BasicAuthData -> IO (BasicAuthResult user)`的封装。  
`BasicAuthData`包裹了前端发送来的用户名和密码（strict `ByteString`类型），我们利用这个参数来验证用户身份  
`BasicAuthResult`有4中情况，分别是
+ `Unauthorized`
+ `BadPassword`
+ `NoSuchUser`
+ `Authorized user`

为了让servant能够自动调用这个函数，我们需要利用`Servant.Server.Internal.Context`。`Context`是一种将值传递给API里面的组合子的手段，我们这里要把`authCheck`函数传递给`BasicAuth`组合子
```haskell
authContext :: Context '[BasicAuthCheck User]
authContext = authCheck :. EmptyContext
```
最终我们要传递的`Context`中包裹的是一个类型层面的列表，里面存放各种值的类型。运算符`(:.)`可以把任意类型的值连接到`Context [*]`的值上去。使用这个运算符我们就可以组合多个Context了。

最后，我们用`serveWithContext`取代`serve`，它的三个参数分别为 api proxy, context, server，例如：
```haskell
main :: IO ()
main = run 8081 $ serveWithContext
  Proxy :: Proxy PrivateAPI
  authContext
  privateServer
```

## Generalized Authentication
TODO
