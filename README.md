----------------------------------------------------
Erlang Log Machine.
----------------------------------------------------

## Intro ##

This application is designed to help to:
 * collect events (i.e. event logging)
 * manage event logging
 * effective access to events logged
 * analyze events in a real time and making a decisions 
in an Erlang clusters. Someone may use this system 
just as a back end for an error logging. Someone may go 
further and construct a system for health monitoring and 
automatic healing feature based this system. Also one of 
the real usages of this system is a quote logging.

## Design ##
Main components of a system are:
 * receiver
 * recorder
 * cacher
 * subscription manager
 * locator
TBD

## Usage ##

### Integrate into your project ###

This is a rebar'ized project, so, if you are already using rebar, just insert a reference 
to this git repo at your rebar.config.
Otherwise clone this repo, and run ``erl -make``.

### Examples ###
TBD