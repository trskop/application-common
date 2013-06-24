Abstract way of building applications that have multiple modes of operation and
configuration passed on command line and/or using configuration files.

Intention is to provide logical separation of action and configuration. Under
action we understand what application will do after command line options are
processed and configuration files loaded. The result of processing command line
options and configuration files we call configuration.

Action has to have a Semigroup instance that provides notion how actions may
change. Both, action and configuration, has to have Default instances. These
together define default behaviour of an application.

For small applications this might be a little overhead. Other packages may be
better for that. For more complicated and simply extensible applications this
library provides very consistent way how to extend it. Adding new action is
simple, changing default mode of operation is also very simple. Since action
and configuration are separated changing one doesn't automatically mean that
the other will have to be changed also.
