# Digipostarkiv

OSX Desktop application to syncronize files to Digipost.

### Features

* Two way sync between local folder (Digipostarkiv) and Digipost (https://www.digipost.no)
* Oauth2 login for Digipost
* OSX status bar application GUI
* Detects changes to files
* Suports Digipost folders
* Will only sync documents marked as origin="uploaded"
* Checks for local changes every 10 seconds, server every minute.

### TODO

* Constantly monitor local files instead of polling
* Consider database instead of flat file
* Show recentlyg synced files in GUI

### Getting started

* install ghc >= 7.8.1
* install cabal >= 1.20
* git clone this repo
* cabal sandbox init
* cabal install --only-dependencies
* cabal build
* open xcode project
* Set GHC_VERSION variable or change the path to where ghc is installed under build properties in xcode
* build and run
