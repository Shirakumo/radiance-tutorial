# Part 1 
## Introduction
While many things in Radiance will work very similarly to other web frameworks, others will not. Either way, you should probably either follow the tutorial along step by step, or in the very least keep the [documentation](https://shirakumo.github.io/radiance) open to reference functions alongside.

In this tutorial we're going to build a fully-fledged paste service from the ground up. I'm going to show this in the way that any other application might be designed. As such, we're going to start out simple and expand from there, testing things along the way.

It is expected that you have a moderate understanding and amount of experience with Common Lisp, know how to create basic projects, and how to manage systems, dependencies, and packages.

This tutorial was written for Radiance 1.0; it may not work for later versions.

## A Short Roadmap
A first step should probably be to consider what kind of capabilities the application should have and in what way they are presented to the user. A paste service is relatively clearly defined, so this isn't much of a problem.

In the end, we'll want:

* A capable editor to write text with (syntax highlighting)
* A way to post the text under a unique and short URL
* Being able to post the texts both anonymously and under a profile
* Some kind of spam protection
* Seeing pastes on people's profile and a public list
* Allowing the pastes to be protected by a password and unlisted
* Being able to access everything programmatically through a REST API
* A way to edit and delete pastes after the fact

The first step however should be much more minimal. We'll settle with the following:

* A site with a text area that allows you to post
* A site that allows you to view a previous post

We'll make short mission statements like that for each part of this tutorial to outline what we want to have accomplished by the end of it.

## Preparing a Module
Radiance has a standard project structure that we're going to follow for now. So, we're creating a new directory somewhere within Quicklisp's local projects or somewhere that ASDF can find it like so:

```
plaster/
plaster/static/
plaster/template/
plaster/plaster.asd
plaster/module.lisp
plaster/frontend.lisp
```

The ASDF system definition should contain your average definition info, with some extras for Radiance's bookkeeping. Thus, `plaster.asd` should contain the following:

```common-lisp
(in-package #:cl-user)
(asdf:defsystem #:plaster
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :components ((:file "module")
               (:file "frontend"))
  :depends-on (:r-clip))
```

The `:defsystem-depends-on` and `:class` parts make sure to register the system with Radiance's modules. We'll also use `r-clip`, which is a convenience extension for the [Clip](https://shinmera.github.io/clip) template system. You can use any template system we want to for your own projects. For this, we'll go with Clip.

Next, we'll need the `module.lisp` file, which is basically an extended package definition.

```common-lisp
(in-package #:rad-user)
(define-module #:plaster
  (:use #:cl #:radiance))
```

The `define-module` macro works just as `defpackage` does, but allows some extensible extra options for modules and makes sure to register the package as a proper module within Radiance.

## Setting Up Pages
Now that we have all the boilerplate basics set up and ready, we can start with our actual application. To begin with we'll want two pages, one to make a paste, and one to view a paste. To do this, we'll populate the `frontend.lisp` like so.

```common-lisp
(in-package #:plaster)

(define-page edit "plaster/edit" ())

(define-page view "plaster/view/(.*)" (:uri-groups (id)))
```

As they stand, the definitions won't actually deliver any content yet. Don't worry, we'll get around to that in a minute. For now, let's just see what this does.

In Radiance, the code that is executed to satisfy any particular request is decided by a mechanism called "uri dispatching". Therein a request's target URL is compared against a sequence of dispatchers. As soon as a dispatcher's URI matches gainst the request, the dispatcher is executed and the dispatching ends.

For our first page, `edit`, we define `plaster/edit` which decomposes into an URI with one subdomain, `plaster`, and a path component of `edit`. For the second page, `view`, the resulting URI has the same subdomain, but its path component is a bit more complex as `view/(.*)` is a regular expression. The path component is the only part of a URI that can contain a regular expression.

Following the URI is a list of page options, which is used in the `view` page to define a capture variable for the regex group. In the page's body we can now refer to the value captured by the group with the `id` variable.

You may be wondering why we're using a subdomain for both of our URIs. Radiance's convention is that each application should put all of its pages onto a subdomain named after the module. This avoids pages from clashing with each other. If you don't want to require the user to access the application through a subdomain, you can fix that later by using a route. We'll get to routing in a later part. For now, just know that there's a good reason for this convention.

## Designing a Template
Let's move on to templating. We'll want to see something on those pages after all. I don't expect you to be familiar with Clip, so I'll give a short crash course here as well. For more information, see its [documentation](https://shinmera.github.io/clip).

Create a file `plaster/template/edit.ctml` and edit it to contain something similar to this.

```html
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <meta charset="utf-8" />
    <title>Plaster</title>
    <link rel="stylesheet" type="text/css" href="../static/plaster.css" @href="/static/plaster/plaster.css" />
  </head>
  <body>
    <header>
      <h1>Plaster</h1>
      <nav>
        <a href="#" @href="/edit">New</a>
      </nav>
    </header>
    <main>
      <form class="edit" @action="/api/plaster/new" method="post">
        <input type="text" name="title" placeholder="Untitled" />
        <textarea name="text"></textarea>
        <input type="submit" name="action" value="Post" />
      </form>
    </main>
    <footer>
    </footer>
  </body>
</html>
```

For the most part this is standard HTML5. The only special things in the template so far are the `@href` and `@action`. These special attributes use URIs as their values, which are then translated and used as replacements for the `href` and `action` attribute values respectively once the template is processed. This translation is part of Radiance's routing system and ensures that links within your templates are turned into references that work on any possible server setup.

Since this file is mostly just HTML, you can open it up in your browser and have a look. It's pretty barren for now, but we can fix that easily enough by writing some CSS. To do so, we'll create the file that we already referenced in the HTML file, namely `plaster/static/plaster.css`.

```CSS

```
