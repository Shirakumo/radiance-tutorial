# Part 0
## Introduction
In Radiance, many things work similarly to the way they work in other web frameworks, but some things do not. No matter which aspects of Radiance you decide to use, you should probably try performing the steps in this tutorial yourself, while keeping open the [Radiance documentation (programmer's reference)](https://shirakumo.org/docs/radiance) so you can study the [reference definitions](https://shirakumo.org/docs/radiance/#index) of Radiance's functions, macros, etc. as you first encounter them in this tutorial. The [reference definitions](https://shirakumo.org/docs/radiance/#index) comprise a comprehensive index of Radiance's functions, macros, generic functions, and of the special variables, routes, options, hooks, resources, conditions, and field types of the thirteen built-in Radiance modules. Each definition includes cross references to related symbols and a link to the defined symbol's source code.

Besides the reference definitions, the documentation includes a basic quick-start guide, a pointer to this tutorial, a brief capsule of [pointers to Radiance's development](https://shirakumo.org/docs/radiance/#system), and an important, in-depth overview of [Radiance's concepts and parts](https://shirakumo.org/docs/radiance/#1._radiance_concepts_&_parts) that you should at least skim or scan.

This tutorial will guide you in building a fully fledged paste service from the ground up. We're going to do it the same way any application might be designed. Thus we will start out simple and successively elaborate, testing functionality along the way. To be comfortable following the tutorial, you should have an intermediate level of experience with Common Lisp, know how to create basic Lisp projects, and know how to manage Lisp systems, dependencies, and packages. The tutorial was written for Radiance 1.0; it might not work for later versions and definitely will not work for earlier versions.

## Dependencies
It is expected that you have a working Common Lisp environment, including Quicklisp and ASDF. If you do not, consider using [Portacle](https://shinmera.com/docs/portacle) for quick setup.

## Installing Radiance
Due to various circumstances, Radiance is not provided through the standard Quicklisp distribution. However, Quicklisp supports multiple distributions (called "dists"). Adding a distribution to Quicklisp is a matter of just one line. The following should be sufficient to install Radiance:

```common-lisp
(ql-dist:install-dist "http://dist.tymoon.eu/shirakumo.txt")
(ql:quickload :radiance)
```

The Shirakumo dist will sometimes provide more up-to-date versions of other libraries than the standard Quicklisp distribution provides.

## Following the Tutorial
The first seven parts of tutorial deal with the incremental creation of your application's features. You are not expected to go through all the parts at once, but you are expected to copy the code from a part of the tutorial, experiment with it, get it to work, and move on to the next part. Merely reading through the tutorial will probably only give you a fraction of the total learning experience.

The last part, [Part 8](Part%208.md), addresses general questions that might come up.

## Resources & Documentation
Here are links to relevant documentation and resource pages that will be useful to refer to as you go through this tutorial.

* [Radiance](https://shirakumo.org/docs/radiance)
* [Interface Definitions](https://shirakumo.org/project/radiance/blob/master/standard-interfaces.lisp)
* [Clip](https://shinmera.com/docs/clip)
* [lQuery](https://shinmera.com/docs/lquery)
* [LASS](https://shinmera.com/docs/LASS)
* [Crypto-Shortcuts](https://shinmera.com/docs/crypto-shortcuts)
* [CodeMirror](http://codemirror.net/doc/manual.html)
* [Ubiquitous](https://shinmera.com/docs/ubiquitous)

## Tutorial Roadmap
A good roadmap for the creation of a paste service can be based upon a list of the application's capabilities and interactions with the user. A paste service is relatively well defined, so this isn't much of a problem. Our roadmap will include the creation of:

* A capable editor to write text with syntax highlighting
* A way to post the text under a unique and short URL
* The ability to post texts both anonymously and under a profile
* Some kind of spam protection
* A way to see pastes on people's profiles and on a public list
* A way to protect pastes by password and unlist them
* Programmatic access to everything through a REST API
* A way to edit and delete pastes after the posting them

However, the mission of the tutorial's Part 1 will only be to create:

* A site with a text area that allows you to post
* A site that allows you to view a previous post

Each part of this tutorial except Part 8 begins with a limited mission like this to add a few visible features to the application.

With this introduction out of the way, you are ready for Part 1, where you will create the posting and viewing sites.

[Part 1](Part%201.md)
