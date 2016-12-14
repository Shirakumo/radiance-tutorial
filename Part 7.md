# Part 7
[Part 0](Part 0.md) [Part 1](Part 1.md) [Part 2](Part 2.md) [Part 3](Part 3.md) [Part 4](Part 4.md) [Part 5](Part 5.md) [Part 6](Part 6.md)

## Resources & Documentation
Here are links to relevant documentation and resource pages that will be useful to refer to for this tutorial.

* [Radiance](https://shirakumo.github.io/radiance)
* [Interface Definitions](https://github.com/Shirakumo/radiance/blob/master/standard-interfaces.lisp)
* [Clip](https://shinmera.github.io/clip)
* [lQuery](https://shinmera.github.io/lquery)
* [LASS](https://shinmera.github.io/LASS)
* [Crypto-Shortcuts](https://shinmera.github.io/crypto-shortcuts)
* [CodeMirror](http://codemirror.net/doc/manual.html)
* [Ubiquitous](https://shinmera.github.io/ubiquitous)

## A Short Roadmap
This is the last entry in this tutorial. Congratulations for making it this far!

* Ideas for Improvements
* Being a Sysop
* Untouched Areas

Time to wrap it all up!

## Ideas for Improvements
The application is mostly done, but there are some things that could be added still. I've omitted them from this tutorial to make it a bit more brief, since they're nothing substantially new that would touch on new areas of Radiance. As such, they're improvements you should be able to make completely on your own, if you so desired.

* Spammer protection and/or Captchas
* Customisation of the title
* Theme switching
* Paste access sharing
* Automatic refreshing of lists and pastes
* Adjusting timestamps for local browser time

There's probably a lot more ideas that could be implemented still. I'm sure you can think of some on your own as well.

## Deployment
We're going to take off our developer boots now and instead put on the sysop shoes. We want to run a new website that provides a paste service. Fortunately for us, some kind soul has already written all the software to do this, so all we need to do now is to run it on our server.

Now, we're already running a webserver on our machine to provide some static HTML files. Naturally we still want to do so, and we don't really want to switch servers around. Instead, the paste service should just be provided on a subfolder. More specifically, while our website is provided at `guybrush.freedns.example/`, the paste service should be at `guybrush.freedns.example/paste/`.

First we need an installation of Radiance itself. Fortunately enough, getting one is rather simple and merely requires a running Lips implementation and an active internet connection. Lets get that set up.

```shell
curl -O https://raw.githubusercontent.com/Shirakumo/radiance-bootstrap/master/bin/radiance-bootstrap.lisp
sbcl --script radiance-bootstrap.lisp
```

Substitute for other means of downloading and running the script as desired. It'll ask you some questions first-- most importantly where it is supposed to install to and what domains it will be reachable through. Radiance needs to know this in order to be able to properly distinguish between the part of a domain that it can have control over (subdomains) and the parts it cannot (top-level domain).

For our example here, during the installation we'll tell it that `guybrush.freedns.example` and `localhost` should be the accepted domains. We'll also run our instance on the standard port of `8080`. Now that we have Radiance installed, we'll want to install Plaster. The setup helpfully points out the place where modules should go; all we need to do is download or clone a copy of Plaster, place it in there, and configure it to be loaded.

We can do so in the configuration file that the installation will have created for us. It should be in `config/default/radiance-core/radiance-core.conf.lisp`. There it should have a list called `:startup` that contains the names of the modules that should be loaded. We can simply add `:plaster` to the end of that list.

So far so good. The next step is to configure our existing HTTP server to proxy requests on `/paste/` to `localhost:8080`. That way, the Radiance installation will be reachable without the need for the special port.

However, there is a slight problem. When Radiance receives requests, it won't be able to dispatch them properly yet. Let's illustrate that with an example. Say there's a request coming in to `guybrush.freedns.example/paste/new`. The primary HTTP server picks it up and proxies it to Radiance. Radiance in turn translates the request URL into the URI `/paste/new` because it knows that `guybrush.freedns.example` is a top-level domain that we cannot change. It then tries to dispatch on it. Unfortunately, no matching dispatcher will be configured, since the only pages we have are listening on `/static/`, `/api/`, `plaster/new`, and `plaster/view`.

Hang on though. So far we've been able to look at our pages just fine despite this weird page mapping. Indeed so, but in order to do so we've been making use of a special mechanism in Radiance that can simulate subdomains. This mechanism, the `/!/` path prefix, is provided through a route. We can solve our current mapping problem using routes as well. More specifically, what we want to do is to map all the requests Radiance receives on the path prefix `/paste/` to a request on the subdomain `plaster` with that prefix stripped from the path.

The configuration file also has a list for routes that we can use to configure our own. Adding the following two entries to the end of that list should already suffice.

```commonlisp
(paste :mapping "/paste/(.*)" "plaster/\\1")
(paste :reversal "plaster/(.*)" "/paste/(.*)")
```

This will do exactly what we need, in two steps-- a route that performs the translation from the outside world into Radiance's application domain, and a route that does the opposite. It is important that both exist. Without the first one, pages would be unreachable. Without the second one, links within the emitted page content would point to the wrong places.

Essentially Radiance separates concerns into two worlds. One world that a user of the website sees, and another that the application sees. The routing mechanism is responsible for bridging the gap between the two. This is not only a good idea, but also absolutely necessary in order to allow you to write applications that can be used on any setup. Anything else would soon hit limitations that cannot be accounted for anymore, and would thus require modification of the application's source.

The routing system can be used for much more than that, though. The virtual domains route with `/!/` is one example of that. Another example would be to rewrite URIs that point to static files such that they are fetched from a different server altogether. You can also implement strategies that provide different services on different ports, etc.

Now that all is set up and ready to go, Radiance can be started using the `start.lisp` file that was generated as part of the installation.

```shell
sbcl --script start.lisp
```

This should set everything up and present you with a REPL for interactive purposes. If you want to run Radiance in the background as a service, you'll have to use your knowledge as an administrator to get that done. Usually using something like `nohup`, `screen`, `tmux`, or similar will do just fine. Maybe also with a wrapper that restarts it automatically in case of a crash. If you need to change the configuration on the fly and don't want to restart Radiance wholesale, invoking `reload-environment` at the REPL should probably do most of what you need. Otherwise you might need to `shutdown` and `startup` again, or in the worst case, restart the Radiance process entirely.

For most purposes, changing the configuration files inside `config/` should give you enough flexibility to get the setup you want. For anything more, you can edit the `setup.lisp` file in the Radiance root, which will give you the full Lisp language to do whatever you need. Note that there are more powerful route definition constructs than the simple string-based routes above. Routes can perform entirely arbitrary things, should the need arise. See `define-route` and the associated macros.

As an administrator, you will probably also want to get the hang of some of the interfaces-- most importantly the `user`, `auth`, and `session` interfaces that allow you to manage access to the individual parts. While there are things like the `admin` interface that will give you a website to do much of everything in the browser, it is still sometimes useful, or even faster, to be able to do the work from the REPL. As such, while knowledge of programming and Radiance should not be necessary to run a Radiance setup, it can be tremendously helpful.

## Untouched Areas
While this tutorial touched on a lot of things Radiance offers, it naturally didn't explore everything. There's still a few more standard interfaces that Radiance offers, further things you can do with requests and responses, extension mechanisms for options and resources, custom interface definitions, etc.

Depending on your needs and goals when you write your own application, you may find some of those features useful. Hopefully the [documentation](https://shirakumo.github.io/radiance) will help you find what you're looking for.

We've also not touched on interface implementation at all. Usually you won't need to do anything with that either unless you, as a sysop, have needs that aren't met by the standard contribs. However, there isn't really much to the implementation aspect beyond reading the specification for the respective interface and writing the necessary code. You can have a look at the [existing implementations](https://github.com/Shirakumo/radiance-contribs) to see what an implementation might look like.

## Final Thoughts
