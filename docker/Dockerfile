# Use an official Python runtime as a parent image
FROM continuumio/anaconda3

# Set the working directory to /app
WORKDIR /app

# Copy the current directory contents into the container at /app
ADD . /app

# ENV DEBIAN_FRONTEND noninteractive
# RUN apt-get update && \
#     apt-get -y install gcc mono-mcs && \
#     rm -rf /var/lib/apt/lists/*

# Install any needed packages specified in requirements.txt
RUN pip install --trusted-host pypi.python.org -r requirements.txt

RUN apt-get -y install emacs24-lucid
