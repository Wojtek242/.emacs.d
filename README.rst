EModule
=======

EModule (Emacs Module) - is a small add-on to Emacs that helps manage personal
configuration by splitting it into manageable modules.  It goes beyond simply
splitting configuration across multiple files by also managing the installation
and removal of packages.  In the future, it should also support package updates
and roll-backs.

This was inspired by Spacemacs' automatic package management with layers.
However, adding additional configuration to an existing layer, or creating a
new layer required some ramp-up in how they work.  By developing something
simpler, the hope was to have something close in simplicity to just writing a
basic init file, with the additional benefit of automatic package installation
and removal.

Installation
------------

This is not available as a package (yet), as I have yet to implement any update
features.  In the meantime to install, simply download the ``emodule.el`` file
and load it from you init file.

I recommend cloning it as a subtree of your project.  From within your
``.emacs.d`` directory run:

::

   git subtree add --prefix=emodule https://github.com/Wojtek242/emodule.git master --squash

This will create the ``emodule`` subdirectory in your project with this
repository checked out in it.  You will then be able to pull updates by
running:

::

   git fetch https://github.com/Wojtek242/emodule.git
   git subtree pull --prefix=emodule https://github.com/Wojtek242/emodule.git master --squash

Usage
-----

Import the package:

::

   (add-to-list 'load-path "~/.emacs.d/emodule")
   (require 'emodule)

Define one or more modules in the modules directory (by default
``~/.emacs.d/modules``).  The module ``MODULE``, is expected to be defined in a
file called ``MODULE.el`` which will define the list of required packages in
``emodule/MODULE-packages`` and any accompanying initialisation in the function
``emodule/MODULE-init``.

Now simply invoke the ``emodule/init`` function from your main init file

::

   (emodule/init '(MODULE
                   ...))

Which will install all the required packages (including all dependencies),
remove any packages that are no longer required (i.e. packages that are no
longer explicitly listed as required and are not a dependency of some other
required package), and invoke all the initialisation functions.

There is also ``emodule/init-debug``, which will skip installing/removing
packages so you can comment out modules to help you in debugging your init
file.

You can find an example of how it's used in my own config_.

Under development
-----------------

- I expect to add automated update functionality as well as roll-backs in case
  an update breaks the configuration.


.. _config: https://github.com/Wojtek242/.emacs.d
