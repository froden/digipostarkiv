# Digipostarkiv

Simple work in progress file sync client for files uploaded to Digipost

### Getting started

* install ghc	```brew install ghc```
* install haskell platform	```brew install haskell-platform```
* cabal configure
* cabal build
* open xcode project
* build and run

### Current status

* OSX status bar application GUI
* Oauth2 login for Digipost
* Two way sync between local folder (Digipostarkiv) and Digipost archive
* Will only sync documents marked as origin="uploaded"
* Uses .sync and a three way diff to detect deleted files
* Files can only be deleted from Digipost (to prevent accidentally deleting all files in archive)
* Checks for changes every 10 seconds

### TODO
* Handle updated files
* Status bar icon
* Sync status indication
