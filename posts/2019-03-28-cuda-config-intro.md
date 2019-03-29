---
title: CUDA 新手入门配置 (CMake)
---

## CMake

从CMake 3.10开始，FindCuda被弃用，转而把CUDA作为一种和C++同等地位的语言来支持，我们只需要在`project`命令的`LANGUAGES`列表里面写上`CUDA`即可。例如：
```cmake
project(cuda_demo LANGUAGES CUDA CXX)
```

然后，`CUDA`源代码文件名后缀采用`cu`，像其他C++文件一样列在`add_executable`/`add_library`的列表里面就可以了

## 特殊头文件

CUDA的IDE支持比较糟糕。CLion没有提供官方支持，Nvidia宣称支持Visual Studio，但是对于`<<<>>>`语法会报错（实际编译时不会报错），是因为这些IDE的实时提示功能不完全支持CUDA的语法。但我们有一系列的方法，可以让IDE不弹出恼人的错误提示

首先，在`*.cu`文件顶部添加这两个头文件包含
```c++
#include <cuda_runtime.h>
#include <device_launch_parameters.h>
```
这样，对于`threadIdx`等内置变量，`cudaMalloc`等函数，IDE就可以正确识别了
为了让CMake能找到这两个头文件，你可能还需要在`CMakeLists.txt`中添加一条`target_include_directories`，例如：
```cmake
# cuda_demo 是某个包含cuda代码的target（可执行文件或库等）
target_include_directories(cuda_demo
  PRIVATE ${CMAKE_CUDA_TOOLKIT_INCLUDE_DIRECTORIES}
  )
```

完整的示例`CMakeLists.txt`。这里项目名是`cuda_demo`，有一个可执行目标，名称也是`cuda_demo`
```cmake
cmake_minimum_required(VERSION 3.13)
project(cuda_demo LANGUAGES CUDA C CXX)

set(CMAKE_CXX_STANDARD 14)

add_executable(cuda_demo
        src/main.cu
        )
target_include_directories(cuda_demo
        PRIVATE ${CMAKE_CUDA_TOOLKIT_INCLUDE_DIRECTORIES}
        )
```

## 项目创建和编译

首先创建一个文件夹`cuda-demo`，然后在里面创建文件`CMakeLists.txt`，把上面的`CMakeLists.txt`的内容粘贴进去。创建子目录`src`，在里面创建源代码文件`main.cu`。你可以在`main.cu`里写一个简单的C++ Hello world。

除非IDE较好地支持CMake，不然需要手动调用CMake创建特定的工程文件。以Visual Studio 2017为例。

- 创建编译目录：在`cuda-demo`里面创建目录`cmake-build`
- 进入`cmake-build`目录，打开命令行，执行`cmake .. -G "Visual Studio 15 2017 Win64" -DCMAKE_BUILD_TYPE=Debug`
- 如果没有错误，将会在当前目录看到Visual Studio的工程文件。找到后缀名为`sln`的文件，用Visual Studio打开（正常情况下双击即可），然后会在Visual Studio里打开这个解决方案
- Visual Studio显示有多个project，我们现在想要运行`cuda_demo`，所以把`cuda_demo`项目设置为启动项目，然后点击上面的绿色箭头运行，即可开始编译并运行
- 以后如果需要添加源代码文件，需要去`cuda-demo/src`目录下创建文件（不要直接在Visual Studio里创建），然后把文件名添加到`add_executable`下面。再在Visual Studio里面执行一次编译，或者重新调用CMake，就可以在Visual Studio里面看到这个文件了

注意事项：如果需要修改CMake的命令行参数，则须清空`cmake-build`目录，才能重新执行`cmake`。而如果命令行参数没有变，只是修改了`CMakeLists.txt`，直接在原来的`cmake-build`目录下执行`cmake`即可。

## CUDA 核函数调用语法的处理

CUDA的核函数调用采用特殊语法`<<<>>>`，如果直接写在IDE里面会报错。另外，`__global__`等关键字有事也会报警告。我们可以用这样一个宏来解决
```c++
#if defined(__JETBRAINS_IDE__) || defined(__INTELLISENSE__)

#define __global__
#define __device__
#define __host__

#define DEVICE_CALL(f, m, n) f

#else

#define DEVICE_CALL(f, m, n) f<<<(m), (n)>>>

#endif
```
下面举例说明如何使用。假定我们有核函数`vector_add`
```c++
__global__
void vector_add(const double *a, const double *b, double *c) {
  // ...
}
```
使用`<<<>>>`语法，这样调用这个函数：
```c++
vector_add<<<blocks, threadsPerBlock>>>(dev_a, dev_b, dev_c);
// blocks 和 threadsPerBlock 分别表示块的数量和每块内的线程数量
```
用上面的宏来改写：
```c++
DEVICE_CALL(vector_add, blocks, threadsPerBlock)(dev_a, dev_b, dev_c);
```
可以看到，`DEVICE_CALL`有三个参数，第一个是核函数名，后两个是原先`<<<>>>`的两个参数。接着，后面重新用一组小括号括住`vector_add`本身的参数

