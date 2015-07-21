Big Bang
========

Usage
-----

### TinyBang toploop

The TinyBang toploop accepts the following command line arguments:

- `--log [<module>=]<log-level>`: If `<module>` is not given, change the log
  level of the whole executable to the given `<log-level>`. If `<module>` is
  given, overwrite the log level for that module.

  Available log levels are:

  - `trace`.
  - `debug`.
  - `info`.
  - `warn`.
  - `error`.
  - `fatal`.
  - `always`.

  The default log level is `warn`.

Setup development environment
-----------------------------

There are two ways to setup your development environment. The first is to use a
container provided by [Docker][what-is-docker] and managed by
[Docker Compose][docker-compose]. That's the recommended approach, but if it
doesn't suit your taste, you can setup manually. The following sections describe
the procedures.

### Managed container (recommended)

1. Install [Docker and Docker Compose][docker-compose-installation]. If you're
   on GNU/Linux, you can choose to install them directly on your machine. If
   you're on Windows or OS X (or GNU/Linux), you might prefer to use a virtual
   machine with those tools already configured. Here are the steps to do that:

   1. Install [VirtualBox][virtualbox].
   2. Install [Vagrant][vagrant].
   3. Install [Docker Compose plugin for Vagrant][vagrant-docker-compose].
   4. Run `vagrant up` to start the virtual machine.
   5. Run `vagrant ssh` to login to the virtual machine and proceed with the
      next steps.

2. Run:

  ```console
  $ docker-compose run --rm bigbang
  ```

The development environment is now setup and should be ready to build and run
the project. Here are some useful things you might want to do next:

- Build:

  ```console
  $ docker-compose run --rm bigbang 'make'
  ```

- Run the tests:

  ```console
  $ docker-compose run --rm bigbang 'make test'
  ```

- Run TinyBang toploop:

  ```console
  $ docker-compose run --rm bigbang 'ocamlrun tiny_bang_toploop.byte [<arguments>]'
  ```

  Refer to the Usage > TinyBang toploop section for more information on available
  arguments.

- Run a console in the container:

  ```console
  $ docker-compose run --rm bigbang 'bash'
  ```

  To exit and terminate the container, run `exit`. To detach and leave the
  container running -- which might be useful if a long running process is active
  --, type `Ctrl-p Ctrl-q`. To reattach later, run `docker ps` to grab the
  container id and run `docker attach <container-id>`.

### Manual

The managed container from the previous section was created using scripts. The
manual setup consists of reproducing them by hand:

1. [Install OCaml and Opam][install-ocaml]. Refer to the `Dockerfile` to check
   the appropriate version. It's recommended that you use `opam switch` to
   create an isolated installation.

2. Use `opam install` to install the dependencies. Refer to the `Dockerfile` to
   get a list of required packages.

3. Refer to `docker-compose.yml` at the `command` entry to learn all the
   necessary steps to setup and configure the build environment.

The development environment is now setup and should be ready to build and run
the project. Here are some useful things you might want to do next:

- Build:

  ```console
  $ make
  ```

- Run the tests:

  ```console
  $ make test
  ```

- Run TinyBang toploop:

  ```console
  $ ocamlrun tiny_bang_toploop.byte [<arguments>]
  ```

  Refer to the Usage > TinyBang toploop section for more information on available
  arguments.

Project Organization
--------------------

[Big Bang][big-bang] is written in [OCaml][ocaml] and uses [OASIS][oasis] to
manage the build process. Although [OASIS][oasis] provides the ability to
customize the build process by making changes to the generated `setup.ml` file,
we currently do not use this mechanism and the setup file is excluded from the
repository.

Source files in the project appear under the `src/` directory. It contains a
number of subdirectories, each of which represents a single library in the
`_oasis` configuration file. Those directories contain only a flat presentation
of [OCaml][ocaml] source files.

Coding style
------------

This project uses the
[standard coding conventions for the OCaml language][ocaml-coding-style],
particularly with regard to identifier naming.

Add a module
------------

All modules need to be listed on `_oasis` under the `Modules` key for the given
`Library`.

Add test
--------

Add a module to the `test/` folder that defines `tests`. Then add the module to
`test/test_tiny_bang.ml`.

Add dependency
--------------

Dependencies are managed by [OPAM][opam] and the consistency of the dependencies
(i.e. guaranteeing that the packages have versions that work well together) is
kept by providing a build environment via the
[`leafac/big-bang` Docker image][docker-image-big-bang].

Even if you don't use the container for development, you should update the
image. This way you keep the other developers that are using the container
happy, as they won't even need to know about the environment change. And, even
more important, you keep an *executable documentation* of the build environment.

I'm going to drive the guide on how to add dependencies by example. We're going
to add the [`lwt` package][lwt]:

1. Check the current version of the image in `docker-compose.yml` under the
   `image` entry. Currently this number is `0.0.1`.

2. Create a new version number, according to
   [Semantic Versioning][semantic-versioning]. As this is not going to be a
   breaking change, the new version number is `0.0.2`. *Don't change the
   `docker-compose.yml` file yet!* We're going to come back to this once we have
   the image ready.

3. Install the dependency in the container:

   ```console
   $ docker-compose run bigbang 'cd /home/opam/opam-repository && git pull && opam update -u -y && opam install lwt'
   ```

4. Fetch the `NAME` of the container that contains the dependency:

   ```console
   $ docker ps -a
   ```

   The `NAME` has the form `bigbang_bigbang_run_<number>`. You should choose the
   one that ran the `COMMAND` `cd /home/opam/opam-repository && git pull && opam
   update -u -y && bash -ic 'opam install lwt'`. In my case, it was
   `bigbang_bigbang_run_8`.

5. Commit the changes:

   ```console
   $ docker commit --author="Leandro Facchinetti" \
                   --message="Install lwt" \
                   bigbang_bigbang_run_8 leafac/big-bang:0.0.2
   ```

   Note that you want to change the `author`, the `message`, the `CONTAINER
   NAME` and the `TAG` version.

6. Push the changes so that they are available for other people on the team:

   ```console
   $ docker push leafac/big-bang:0.0.2
   ```

   **Notice**: This makes the image available to the world. Make sure your
   changes don't contain any private information about yourself and the project!

7. Edit the `Dockerfile` in the line that runs `opam install` to add the
   dependency. This step is important for two reasons. First, people that don't
   use the container have an executable documentation of the dependencies they
   should manually install. Second, we can rebuild the development image from
   scratch if needed with `docker build --tag leafac/big-bang:<version> .`.

8. Edit the `docker-compose.yml` to refer to the new version `0.0.2`.

9. Commit and push your changes to the source repository.

From now on, everyone is going to build using the modified environment you just
created with the new dependency.


[what-is-docker]: https://www.docker.com/whatisdocker/
[docker-compose]: http://docs.docker.com/compose/
[docker-compose-installation]: https://docs.docker.com/compose/install/
[install-ocaml]: http://ocaml.org/docs/install.html
[oasis]: http://oasis.forge.ocamlcore.org/
[opam]: https://opam.ocaml.org/
[docker-image-big-bang]: https://registry.hub.docker.com/u/leafac/big-bang/
[lwt]: https://opam.ocaml.org/packages/lwt/lwt.2.4.8/
[semantic-versioning]: http://semver.org/
[ocaml-coding-style]: http://caml.inria.fr/resources/doc/guides/guidelines.en.html
[big-bang]: https://big-bang-lang.com
[ocaml]: http://ocaml.org/
[virtualbox]: https://www.virtualbox.org/
[vagrant]: https://www.vagrantup.com/
[vagrant-docker-compose]: https://github.com/leighmcculloch/vagrant-docker-compose
