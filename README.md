Erlang Log Machine
----------------------------------------------------

## Intro ##

The logmachine is an application which is designed to gather events occurred in a cluster 
and effective delivery those events to some other agents (apps) processing them in a 
real time or later. The logmachine have its powerful enough API for event delivery. This API
includes an ability to receive a data set (events) starting from a specified time instant as a
lazy list (see zlists). Also there is an ability for some Erlang process to subscribe for 
events also starting from a specified time instant, hence there is an ability for some 
process to receive events missed since the last session and proceed with new events transparently.
So currently logmachine may be considered like a kind of MOM. There is also an intention 
to use logmachine in a heterogeneous environment i.e. with other programming languages. For this 
purpose a restful API planned, and probably some binary API.     

## Design ##

### Data storage organization ###

Logmachine may run a several instances. Each instance is a kind of data base of events. 
Instance have its name. For example if we want to capture all error, warning, info, SASL or 
OS alarm (see os_mon) events we may configure an instance with name 'error_logger' be cause 
all listed kinds of events reported with standard event manager registered under name 'error_logger'.
Meanwhile, for a business events like exchange quotes we may configure something like 'quotes'.

When event received by logmachine it is time stamped and stored in a persistent log and a RAM cache.
 
Of cause event may already have its own timestamp but currently, for simplicity, logmachine closes 
eyes on this fact.   

### Process organization ###

Main components of a system are:

 *   receiver
 *   recorder
 *   cacher
 *   subscription session
 *   locator

### Receiver ###

Each logmachine instance have its own receiver process which is globally registered with alias like 
``{InstanceName :: atom(), N :: non_neg_integer()}`` where N starts from 0. For example mentioned 
above it will be a {error_logger, 0}. This process is an entry point to logmachine instance, so if 
we want to notify logmachine about some event directly lets do: 
```erlang
ReceiverGlobalAlias = {error_logger, 0},
Event = some_error_event,
gen_server:cast({global, ReceiverGlobalAlias}, Event).
```

### Recorder ###

Each logmachine instance have its own recorder which persists events to log. Also this recorder 
is responsible for log rotation and archiving. Currently the only storage back end may be used 
``disk_log``. This solution have its strong and weak sides. In future (hope not so far future) 
there will be support for other back ends. 

### Cacher ###

Each logmachine instance have its own RAM cache. It is a fast low latency store based on ``ets``
that keeps a most recently arrived events e.g. for last 1 hour. This facility provides a high 
performance for data access to most recent events.

### Subscription session ###

When some process want to receive events form logmachine it may call for subscription. If it 
wants to obtain also some missed events in past then a subscription session process created.
This process provides a smooth (gap less) transition from reading events from log to just 
retransmitting new coming events to subscriber. This is not necessary when only newest 
events wanted. This mechanism gives a chance to build a fault tolerant distributed systems. 
For example, if a network split takes place between logmachine and subscriber, then if subscriber 
tracks a last timestamp of received event it just re-subscribes with that timestamp.  

### Locator ###

Each logmachine instance have its own locator process. The locator seeks for an event manager 
specified in a configuration for each visible node and when node is 'up'. When locator locates 
an event manager it installs its own event handler to it which translates events to a receiver 
of its instance. This solution have its strong and weak sides. Also this behavior is optional, 
it is activated only it is configured.

## Usage ##

### Integrate into your project ###

This is a rebar'ized project, so, if you are already using rebar, just insert a reference 
to this git repo at your rebar.config.
Otherwise clone this repo, and run ``erl -make``.

### Examples ###
TBD