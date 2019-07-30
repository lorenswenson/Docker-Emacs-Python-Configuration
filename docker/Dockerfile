# Use an official Python runtime as a parent image
FROM continuumio/anaconda3

# Set the working directory to /app
WORKDIR /app

# Copy the current directory contents into the container at /app
ADD . /app

# Add IPYTHON3
RUN apt-get update
RUN apt-get install -y ipython3

# Add pip
RUN apt-get install -y python-pip

# Install any needed packages specified in requirements.txt
RUN pip install --trusted-host pypi.python.org -r requirements.txt

RUN apt-get -y install emacs24-lucid
