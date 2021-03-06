# Docker + Emacs + Python: Configuration

Docker solves a few typical problems:

* You've just installed a new OS and you start building a development environment.  However, the OS already has an old version of a library you want to use.  You install over the existing version and break the OS.  
* You develop some code and want to send it to another developer.  However, it has a large set of dependencies.  It would be nice to just send the code along with the environment rather than trying to list every library and its version required to run the code.
* You want to just try out some software without worrying that it will not cleanly uninstall and pollute your OS.

Virtual environments try to solve some of these issue.  However, Docker does this very cleanly and makes it easy to send another developer an already working environment with almost no setup required.  It's a process in the host OS that is isolated from the filesystem by default.  This allows it to be similar to a full virtual machine but much lighterweight.

This repository includes example configuration files for setting up an anaconda based python development environment with Emacs configured as an IDE, all within a docker container mostly isolated from the host OS.

-----

Installation instructions:

1) Install docker: 

[Docker Install](https://docs.docker.com/install/linux/docker-ce/ubuntu/)

2) Clone this repo:

`git clone https://github.com/lorenswenson/Docker-Emacs-Python-Configuration`

3) Change to the docker directory and run

`sudo docker build -t dockerpythonemacs .`

This will build an image with the latest ubuntu + anaconda distribution + tensorflow.
Additional python libraries can be added to those found in requirements.txt.

4) Add calling function and alias to ~/.bashrc

```
docker_emacs () {
    docker run -it \
           --user $(id -u) \
           -e DISPLAY=unix$DISPLAY \
           --workdir=$(pwd) \
           --volume="/home/$USER:/home/$USER" \
           --volume="/etc/group:/etc/group:ro" \
           --volume="/etc/passwd:/etc/passwd:ro" \
           --volume="/etc/shadow:/etc/shadow:ro" \
           --volume="/etc/sudoers.d:/etc/sudoers.d:ro" \
           --volume="/tmp:/tmp" \
           -p 6006:6006 \
           -v /tmp/.X11-unix:/tmp/.X11-unix \
           dockerpythonemacs emacs \
           $@
}

alias pymacs='docker_emacs -geometry 280x80 -fn "terminus-10" -bg black -fg yellow'
```

Note: This does a lot:
* It mounts the user home volume into the docker container which is convenient but is a big hole in the container isolation.
* It allows the container to use the display which is great for matplotlib.
* It maps the port 6006 which allows access to tensorboard at http://localhost:6006
* The alias `pymacs` launches emacs within the container with a specified geometry, font, and colors.
* Only mapped directories, namely those with the home directory, will be available.  You'll need to escape emacs to access host OS files outside these mounted volumes.
* ~/.emacs.d/ will be mounted in the docker container.  This gives access to emacs packages installed on the host OS.

5) Run

`source ~/.bashrc`

6) Emacs in the docker container should now be aliased, so just run `pymacs` from bash

7) Emacs should be configured to use [Elpy](https://github.com/jorgenschaefer/elpy/wiki).  A starting point is to add the code in snippet_init.el to ~/.emacs.d/init.el

-----

If you want to update anaconda run:

`docker pull continuumio/anaconda3`

and then rebuild with

`docker build -t dockerpythonemacs .`

in the docker directory.  Similarly, you need to rebuild the image if you add any packages to docker/requirements.txt

Cheat sheet for keeping installed images tidy: https://www.digitalocean.com/community/tutorials/how-to-remove-docker-images-containers-and-volumes

These include:

* Removing dangling images: `docker system prune`
* Listing images:           `docker images -a`
* Removing specific image:  `docker rmi *image*`



