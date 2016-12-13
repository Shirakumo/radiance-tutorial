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

* Being a Sysop
* Ideas for Improvements
* Untouched Areas

Time to wrap it all up!

## Deployment
We're going to take off our developer boots now and instead put on the sysop shoes. We want to run a new website that provides a paste service. Fortunately for us, some kind soul has already written all the software to do this, so all we need to do now is to run it on our server.

Now, we're already running a webserver on our machine to provide some static HTML files. Naturally we still want to do so, and we don't really want to switch servers around. Instead, the paste service should just be provided on a subfolder. More specifically, while our website is provided at `pester.freedns.example/`, the paste service should be at `pester.freedns.example/paste/`.

TODO: BOOTSTRAP.LISP

## Ideas for Improvements
The application is mostly done, but there are some things that could be added still. I've omitted them from this tutorial to make it a bit more brief, since they're nothing substantially new that would touch on new areas of Radiance. As such, they're improvements you should be able to make completely on your own, if you so desired.

* Spammer protection and/or Captchas
* Customisation of the title
* Theme switching
* Paste access sharing
* Automatic refreshing of lists and pastes
* Adjusting timestamps for local browser time

There's probably a lot more ideas that could be implemented still. I'm sure you can think of some on your own as well.

## Untouched Areas
While this tutorial touched on a lot of things Radiance offers, it naturally didn't explore everything. There's still a few more standard interfaces that Radiance offers, further things you can do with requests and responses, extension mechanisms for options and resources, custom interface definitions, etc.

Depending on your needs and goals when you write your own application, you may find some of those features useful. Hopefully the [documentation](https://shirakumo.github.io/radiance) will help you find what you're looking for.

We've also not touched on interface implementation at all. Usually you won't need to do anything with that either unless you, as a sysop, have needs that aren't met by the standard contribs. However, there isn't really much to the implementation aspect beyond reading the specification for the respective interface and writing the necessary code. You can have a look at the [existing implementations](https://github.com/Shirakumo/radiance-contribs) to see what an implementation might look like.

## Final Thoughts
