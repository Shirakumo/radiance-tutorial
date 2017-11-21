# Part 0
## Introduction
While many things in Radiance will work very similarly to other web frameworks, others will not. Either way, you should probably follow this tutorial along step by step, and in the very least keep the [documentation](https://shirakumo.github.io/radiance) open to reference functions alongside.

In this tutorial we're going to build a fully-fledged paste service from the ground up. I'm going to show this in the way that any other application might be designed. As such, we're going to start out simple and expand from there, testing things along the way.

It is expected that you have a moderate understanding and amount of experience with Common Lisp, know how to create basic projects, and how to manage systems, dependencies, and packages.

This tutorial was written for Radiance 1.0; it may not work for later versions and will most definitely not work for earlier versions.

## Getting Set Up
It is expected that you have a working Common Lisp environment including Quicklisp and ASDF going. If you do not, have a look at [Portacle](https://shinmera.github.io/portacle) for a quick setup.

## Installing Radiance
Due to various circumstances, Radiance is not provided through the standard Quicklisp distribution. However, Quicklisp supports multiple distributions (called "dists") and adding one is just a matter of a single line. The following should be sufficient to install Radiance then:

```common-lisp
(ql-dist:install-dist "http://dist.tymoon.eu/shirakumo.txt")
(ql:quickload :radiance)
```

The Shirakumo dist you are installing here will also sometimes provide more up-to-date versions of other libraries.

## Following the Tutorial
The tutorial is divided up into seven parts, each dealing with adding some features to our application. It is not expected that you go through all the parts at once, but it is expected that you follow along with the code on your own and experiment around with the pieces presented to you. Merely reading through the parts will probably only give you a fraction of the learning experience.

There is also a [part 8](Part%208.md) that addresses general questions that might come up.

With all said and done, you may now move on to the first real part of the tutorial.

[Part 1](Part%201.md)
