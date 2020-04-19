# Hardware and software facilities

## Running Haskell in the lab

We need ghc ([Glasgow Haskell Compiler](https://www.haskell.org/ghc/)) version 8.6.5. This should be the default in the lab. You can check this by running

```
$ ghc --version
````

in a terminal. If you don't have 8.6.5, then do

```
$ module load func-prog
```

But we are going to run the *interpreter* **ghci** more often than the *compiler* ghc. You run it as follows:

```
$ ghci
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
Prelude>
```

More details are [here](Book/slides/PDF/ch2.pdf) and in our textbook, and also in [Learn you a Haskell for Great Good](http://learnyouahaskell.com/starting-out#ready-set-go).

## Get Haskell

* [Haskell Platform](https://www.haskell.org/platform/). This works for Linux, MacOS, and Windows, and you can install from source for other operating systems.

You want to get version 8.6.5.  Other versions above 8.0 will probably work as well, although we haven't tried (and won't, so please report success or failure in this respect).

## Connect to the lab from your own machine

* Make sure you have ssh installed. Google is your friend. And feel free to add information in this file, to help your colleagues, via pull requests.

Then run, from your own machine:

```
$ ssh <your-school-of-cs-login-name>@tw.cs.bham.ac.uk
```

This machine is the gateway to the School (the firewall), but not the machine you will use. To get to a lab machine, run, from there,

```
$ ssh-lab
```

This will connect you to a random lab machine. You can use

```
$ who
````

to see who and how many people are logged in, and if there are too many maybe you want to

```
$ exit
```

and log in to another (random or explicit) lab machine.

Run
```
$ module load func-prog
```
to load GHC, so that you can start running your Haskell code.

## Use a virtual machine

If you don't want to login to a School machine to use the tools we provide, you may instead want to install the same operating system the lab machines use:

```
$ lsb_release -a
LSB Version:	:core-4.1-amd64:core-4.1-noarch
Distributor ID:	CentOS
Description:	CentOS Linux release 7.6.1810 (Core)
Release:	7.6.1810
Codename:	Core
```

You can do this either in a separate partition of your hard drive (or ssd), or in a virtual machine inside your favourite operating system such as [VirtualBox](https://www.virtualbox.org/wiki/Downloads). You will need a [CentOS image](https://wiki.centos.org/Download). The lecturer did this successfully under Ubuntu in one of his personal machines.

Ask for help on [Slack](https://uobcsfp20192020.slack.com) if you have trouble doing this.
Please avoid sending email unless absolutely necessary, as we do not have the resources to deal with queries on an individual basis.
