# Docker + Emacs + Python: Configuration

Docker solves a few typical problems:

* You've just installed a new OS and you start building a development environment.  However, the OS already has an old version of a library you want to use.  You install over the existing version and break the OS.  
* You develop some code and want to send it to another developer.  However, it has a large set of dependencies.  It would be nice to just send the code along with the environment rather than trying to list every library and its version required to run the code.
* You want to just try out some software without worrying that it will not cleanly uninstall and pollute your OS.

Python has a few problems:

* It's kind of a mess.  There's *still* discussion about whether to use python 2 or 3.  I mean, this was an issue 5 years ago and it is still not fully resolved.
* It's strength is the rich ecosystem of libraries.  This is also a weakness as library management can be very complicated.

Virtual environments try to solve this issue.  However, Docker makes this very cleanly.  It's a process in the host OS that is isolated from the filesystem by default.  This allows it to be similar to virtual environment but much lighterweight.

This repository includes example configuration files for easily setting up an anaconda based python development environment and setting up Emacs to work as an IDE, all within a docker container mostly isolated from the host OS.

