# Part 8
[Part 0](Part%200.md) [Part 1](Part%201.md) [Part 2](Part%202.md) [Part 3](Part%203.md) [Part 4](Part%204.md) [Part 5](Part%205.md) [Part 6](Part%206.md) [Part 7](Part%207.md)

This part is a Q&A to answer some common questions and problems you might encounter while developing, or while you followed this tutorial.

## Configuration
### Where are configuration files stored?
The exact location of the files varies per operating system, your own environment, and the currently active environment. You can get the "root" directory as follows:

    (truename (make-pathname :name NIL :type NIL :defaults radiance:*environment-root*))

Every environment will have its own directory in there, and in each environment directory, every module will have its own subdirectory as well. A quicker way to find the current location of the configuration file for a specific module is as follows:

    (truename (radiance:mconfig-pathname :radiance))

### How do I look at the configuration?
You can view the currently set configuration for a module using the standard Common Lisp `describe` on the package/module:

    (describe (radiance:module :radiance))

It will also list the claimed pages and API endpoints, whether the module implements an interface, what permissions it defines, and which hooks are available. If you have ASDF hooked up properly it'll even print information about the system as well.

Otherwise you can simply open the configuration file, or use `mconfig`:

    (radiance:mconfig :radiance :interfaces :database)`

### How do I change the configuration?
The obvious way is to simply change the configuration files on disk. Note that you will need to call `reload-environment` in order for the changes to be picked up by Radiance. Also note that some modules might cache configurations somehow, so the changes may not be immediate even after reloading the environment.

You can also change the configuration directly from Lisp, which will also automatically persist to the configuration file, using `mconfig`:

    (setf (radiance:mconfig :radiance :debugger) NIL)

### Are there configuration formats other than the S-expression based one?
Not out of the box, no. Though you can write your own format. See the documentation of [Ubiquitous](https://shinmera.github.io/ubiquitous) for that.

### What kinds of objects can I store into the configuration?
Most everything will be persisted, so you should be able to just throw values in there in most cases. Some exceptions and limitations exist, but they're laid out pretty precisely in the [Ubiquitous documentation](https://shinmera.github.io/ubiquitous).

## Interfaces and Implementations
### How do I change the implementation of an interface?
You adapt the value in Radiance's configuration and restart Radiance. It is possible for some implementations to be replaced in-flight, but it is not a case that is well supported. Macros and global changes provided by an implementation make it difficult to do this.

As an example, switching the database to the postgres implementation would require this:

     (setf (radiance:mconfig :radiance :interfaces :database) "i-postmodern")

Followed by a restart if Radiance was already started up, or the interface has already been loaded.

### Where do I find available implementations?
Shirakumo provides a number of default implementations for interfaces. Every interface has at least one implementation available. You can find the list on the [radiance-contribs](https://github.com/Shirakumo/radiance-contribs) project page.

Note that anyone else, yourself included, can write new implementations for interfaces, or new interfaces altogether. However, there's no way for us to track that, so unless people tell us about it, we wouldn't know how to find it either.

### An implementation that fits my needs doesn't exist, how do I write it?
The short of it is this: add `(:implements #:interface-name)` to your `define-module` form to inform Radiance that you're ready to provide this interface's functionality. You can then use `(modularize-interfaces:print-interface-stub :interface-name)` to generate code stubs for definitions you need to implement or provide. Note that condition and class definitions usually don't need to be replicated, as they're already provided by the interface definition itself. Finally, read the specification of the interface and all of the definitions it provides, and implement them as required.

You can look at the existing interface implementations in [radiance-contribs](https://github.com/Shirakumo/radiance-contribs) to get a better idea of what it looks like. A lot of the implementations are pretty short, and all in all it's not really a complicated process.

If you do end up writing an implementation, do let us know, though. We'd be very interested to hear about it!

### The interface specification is too restrictive for me, and I need more capabilities. What do I do?
Well, you don't need to use the interface. If you need more precise control, you can just write the functionality yourself or use some other library that provides it. Just note that this means less ability for an administrator to configure your software in the way they might need it, and your software might integrate less well with the rest of the ecosystem. If you don't intend on distributing your application to other people, then naturally you don't need to worry about that, though.

## Assorted Questions
### Do I need to use Clip or CL-WHO?
No! You can use whatever templating system you like. The templating is completely orthogonal to what the framework does. All you need to remember to do, is to use the proper URI translation rules in your links. If you can extend your templating system of choice to handle URL translation using `uri-to-url` properly, then you'll be good to go.

### My page is not being called when I open it in the browser? What is going on?
The first problem is that the routes might be translating the URL into an internal address that isn't what you thought it would be. You can check this with `internal-uri`:

     (radiance:internal-uri "localhost:8080/some/path")

If this path is as you expect, and your page isn't being called regardless, you might be running into a priority issue. For example, if we have the following two page definitions:

    (define-page article "blog/(.+)" () ..)
    (define-page landing "blog/" () ..)

Then the `landing` page would never be called. This is because Radiance automatically computes [an order](https://shirakumo.github.io/radiance/#RADIANCE-CORE:URI-DISPATCHER%3E) for your pages where, if your URI parts are equal except for the path part, then the one with a longer path is going to come first. You can check whether this is happening by calling `list-uri-dispatchers` which will show you all pages with the highest priority one being first.

To avoid this problem, you can explicitly supply a priority number to your page like so:

    (define-page (landing 100) "blog/" () ..)

This will ensure that your page is sorted before, unless of course you also supply a priority to the other page that is equal or higher than this one's.

Finally, if this doesn't help either, you can try to perform the request manually at the REPL and potentially trace or step it to see where it's going wrong. You can do this with the `request` function:

    (radiance:request "blog/foo")

This should return a `response` and a `request` object, or error on some failure. You can retrieve the data of the response with `data` to look at it. You can also supply all of the other possible data parts of a request with keyword arguments.

### The URI translation seems different when I do it on the REPL, what's going on?
When a request arrives from the webserver and calls `request`, it attempts to match all the domains you have configured against the request. If one matches, it strips remembers that domain, stores it in the request object, and then removes it from the request's URI to internalise it. When a URI is then externalised in the context of that request, it looks at the request object and re-attaches the domain the request stored from before.

When you perform a URI translation on the REPL however, you typically do not have a request context, so when you externalise a URI it will simply attach the first domain you configured to it, hoping that that's good enough. However, when you actually perform requests from a different domain than you have configured, this will result in a confusing mismatch.

You can emulate the same request environment by binding `*request*` to an instance of `request` with the correct `:domain` argument and then performing the URI translations.

### How do I configure a new domain for Radiance?
Radiance needs to know all the "top-level domains" that you're going to address your server by, in order to distinguish where subdomains start. To do this, you can simply use `add-domain`:

    (radiance:add-domain "cool.guys.club")

If you test it with `internal-uri` you should find that the address is now properly translated:

    (radiance:internal-uri "everything.cool.guys.club/bla") ; => #@"everything/bla

### Can I ship my application as an executable?
Currently this is not directly supported, as the logic to handle path translation for the necessary assets is not handled. However, we would like to support this in a future version of Radiance. For now, you will have to make do with the bootstrapping process that was shown in the previous part.

### Things are not working as the documentation claims they should. What is going on?
It's probably a bug! Either in the documentation or the implementation. Please [let us know](https://github.com/Shirakumo/radiance/issues/new).

### Should I use a proxy in front of my Radiance installation?
That depends on the server implementation you use, but in general the answer will be yes. Other servers like Nginx are simply much faster at handling SSL and delivering static content that putting them in front and setting up the caching logic will give you a significant performance and security boost.

### Is Radiance fast?
That's a loaded question, and the answer mostly depends on the current setup you're running under. Most of the core Radiance parts have been optimised to a reasonable degree so that they should not bottleneck your setup. *If* your setup is slow, consider benchmarking the individual parts of the whole system to figure out where exactly the pain point is.

## Non-Questions
### I don't like Radiance.
That's fine, there's plenty of other frameworks out there to look at if you don't like this one. If you can be bothered, we'd still like to hear what you have to say about it, so please [let us know](https://github.com/Shirakumo/radiance/issues/new).

### I like Radiance.
That's great! If you end up making anything cool with it, we'd be very interested in [hearing about it](https://github.com/Shirakumo/radiance/issues/new).
