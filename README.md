# Docker + Emacs + Python: Configuration

Docker solves a few typical problems:

* You've just installed a new OS and you start building a development environment.  However, the OS already has an old version of a library you want to use.  You install over the existing version and break the OS.  
* You develop some code and want to send it to another developer.  However, it has a large set of dependencies.  It would be nice to just send the code along with the environment rather than trying to list every library and its version required to run the code.
* You want to just try out some software without worrying that it will not cleanly uninstall and pollute your OS.

Python has a few problems:

* It's messy.  There's *still* discussion about whether to use python 2 or 3.  I mean, this was an issue 5 years ago and it is still not fully resolved.
* It's strength is the rich ecosystem of libraries.  This is also a weakness as library management can be very complicated.

Virtual environments try to solve this issue.  However, Docker does this very cleanly and makes it easy to send another developer an already working environment with almost no setup required.  It's a process in the host OS that is isolated from the filesystem by default.  This allows it to be similar to virtual environment but much lighterweight.

This repository includes example configuration files for easily setting up an anaconda based python development environment and setting up Emacs to work as an IDE, all within a docker container mostly isolated from the host OS.

-----

Installation instructions:

1) Install docker: 

https://docs.docker.com/install/linux/docker-ce/ubuntu/

2) Clone this repo:

git clone https://github.com/lorenswenson/Docker-Emacs-Python-Configuration

3) Change to the docker directory and run

docker build -t dockerpythonemacs .

This will build an image with the latest ubuntu + anaconda distribution + tensorflow.
Additional python libraries can be added to those found in requirements.txt.

4) Add calling function and alias to ~/.bashrc

Add all the code in .bashrc_snippet to ~/.bashrc

5) Run

source ~/.bashrc

6) Emacs should now be aliased, so just run emacs from bash

Note that the function in the .bashrc_snippet will map your ~/.emacs.d directory on your host OS.


-----

If you want to update anaconda run:

docker pull continuumio/anaconda3

and then rebuild.  Similarly, you need to rebuild the image if you add any packages to docker/requirements.txt

Cheat sheet for keeping installed images tidy: https://www.digitalocean.com/community/tutorials/how-to-remove-docker-images-containers-and-volumes

These include:

* Removing dangling images: docker system prune
* Listing images:           docker images -a
* Removing specific image:  docker rmi *image*



