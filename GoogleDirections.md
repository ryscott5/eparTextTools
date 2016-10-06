---
title: "Setup a Google Cloud Compute Instance for Text Analysis"
output: html_document
---

#Startup Script

First, update apt-get, and install java.

```bash
sudo apt-get update
sudo apt-get upgrade
sudo apt-get -y install openjdk-8-jre
sudo apt-get -y install openjdk-8-jdk
sudo apt-get -y install git
```

Next,we add the ability to link to a google nearline server. This way you can store the files cheaply in the cloud. Essentially, you need to set up a cloud storage bucket. We then install the package that links to the cloud storage bucket. This code links to the bucket I made called epartextools.appspot.com. If you have your own bucket, chage that to whatever your bucket is named. Bucket1 is the name of the folder I'm mounting the bucket to in the cloud.

```{r}
export GCSFUSE_REPO=gcsfuse-`lsb_release -c -s`
echo "deb http://packages.cloud.google.com/apt $GCSFUSE_REPO main" | sudo tee /etc/apt/sources.list.d/gcsfuse.list
curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key add -
  sudo apt-get update
sudo apt-get -y install gcsfuse
sudo mkdir bucket1
sudo chmod a+w bucket1
gcsfuse epartexttools.appspot.com bucket1
```

Because we are going to be using R and Python and NLP software, we need to install them. First install libcurl, then r, then gdebi, then rstudio server, then wordnet, then java 8. we then add limxml2. 

```bash
sudo apt-get -y build-dep libcurl4-gnutls-dev
sudo apt-get -y install libcurl4-gnutls-dev
sudo echo "deb http://cran.rstudio.com/bin/linux/debian jessie-cran3/" | sudo tee -a /etc/apt/sources.list
sudo apt-key adv --keyserver keys.gnupg.net --recv-key 381BA480
sudo apt-get update
sudo apt-get -y install r-base
sudo apt-get -y install gdebi-core
wget https://download2.rstudio.org/rstudio-server-0.99.903-amd64.deb
sudo gdebi rstudio-server-0.99.903-amd64.deb
gcloud compute firewall-rules create allow-rstudio --allow tcp:8787
sudo apt-get -y install wordnet
sudo apt-get -y install openjdk-8-jre
sudo apt-get -y install libxml2-dev
sudo apt-get -y install antiword
sudo apt-get -y install xpdf
sudo apt-get -y install libemail-outlook-message-perl libemail-sender-perl
pip install https://github.com/mattgwwalker/msg-extractor/zipball/master
```

Try this command.
```bash
R CMD Sys.setenv(PKG_CONFIG_PATH="/usr/lib/x86_64-linux-gnu/pkgconfig")
```

Next add a password for yourself. I'm ryscott5, replace that with your google id and follow prompts.
```bash
sudo passwd ryscott5
```

Next we are going to install docker and the cliff geoserver.
```bash
sudo apt-get -y install docker.io
sudo git clone https://github.com/johnb30/cliff-docker
cd cliff-docker
sudo docker build -t cliff:2.1.1 .

docker run -p "8080:8080" -d cliff:2.1.1
sudo docker ps
sudo docker stop
```

For SHINY server, follow build instructions:
https://github.com/rstudio/shiny-server/wiki/Building-Shiny-Server-from-Source


Now, you can log into rstudio-server by visiting the IP address for your instance followed by :8787.


Future Code for Tensorflow
```bash
wget https://github.com/bazelbuild/bazel/releases/download/0.2.2b/bazel_0.2.2b-linux-x86_64.deb
sudo dpkg -i bazel_0.2.2b-linux-x86_64.deb
#Then run

wget https://bootstrap.pypa.io/get-pip.py
sudo python get-pip.py
sudo pip install -U protobuf==3.0.0b2
sudo pip install asciitree
sudo pip install numpy
git clone --recursive --recurse-submodules https://github.com/tensorflow/models.git
cd models/syntaxnet/tensorflow
./configure
cd ..

sudo update-alternatives --config java

sudo update-alternatives --config javac
sudo apt-get install python-dev
bazel test syntaxnet/... util/utf8/...
```


