# R Library and DECAF Clever Runtime for REMAP Utilities

This repository provides an R library (`remaputils`) and a DECAF Clever runtime
(`decaf-clever-runtime-remaputils`) for auxiliary DECAF and REMAP routines.

> **TODO:** Add general introduction.

## The R Library

> **TODO:** Add instructions for installation and usage.

## The DECAF Clever Runtime

In addition to the `remaputils` R library, this repository provides a DECAF
Clever runtime, namely `decaf-clever-runtime-remaputils`, as a Docker image. It
can be used as a standalone Docker image for both testing and production
purposes.

The Docker image is built and pushed to following Docker repository:

```txt
registry.docker.decafhub.com/decaf-clever-runtime-remaputils
```

Users can pull the Docker image for a given version:

```sh
docker pull registry.docker.decafhub.com/decaf-clever-runtime-remaputils:v<VERSION>
```

### During Development

It might be a good practice to test `remaputils` in its own runtime during
development.

Let's say that you made a change to `remaputils` R library. Now, you want to
test whether it installs and works correctly in the runtime.

First, build the runtime, ie. its Docker image:

```sh
docker build --build-arg VERSION=testing --tag runtime-remaputils-testing .
```

> **NOTES:**
>
> In the above command, we are passing a Docker build argument via
> `--build-arg`, namely `VERSION`, that will set the runtime environment
> variable `DECAF_CLEVER_RUNTIME_IMAGE_VERSION`. Since this build is not for a
> production release, we are setting it to `testing`.`
>
> Also, we are setting the repository name as `runtime-remaputils-testing`. Note
> that Docker image repository names are quite arbitrary if we are not going to
> push the Docker image to a public or private registry. You can choose a
> different repository name, but then, you should use that very name in the
> instructions below.

The build may take a while, especially when you are building it for the first
time. During subsequent runs, Docker will skip steps which performs the bulk of
the installation, if you did not change dependencies (or `install.sh` and
`Dockerfile`). Subsequent runs are typically completed in about 15-20 seconds.

Once ready, give a try to the Docker image by running it with a simple command
(`env` that prints all environment variables):

```sh
docker run --rm runtime-remaputils-testing env
```

You can use the same Docker image to `check` the R package, too:

```sh
docker run --rm -it -v "$(pwd):/tmp/remaputils" runtime-remaputils-testing R --quiet --vanilla -e 'devtools::check("/tmp/remaputils")'
```

Indeed, you can even simulate a DECAF Clever job. See
[./var/clever/examples/basic/README.md](./var/clever/examples/basic/README.md)
how you can do it.

### After Release

After you make a new release, you can issue the following command to rebuild the
runtime by replacing `<NEW-VERSION>` with the version of the release:

```sh
docker build --build-arg VERSION=<NEW-VERSION> --tag registry.docker.decafhub.com/decaf-clever-runtime-remaputils:v<NEW-VERSION> .
```

For example, if the version of the new release is `0.0.1`:

```sh
docker build --build-arg VERSION=0.0.1 --tag registry.docker.decafhub.com/decaf-clever-runtime-remaputils:v0.0.1 .
```

Then, you can push the Docker image built to the Docker registry so that others
can start using it in production, again, by replacing `<NEW-VERSION>` with the
version of the release:

```sh
docker push registry.docker.decafhub.com/decaf-clever-runtime-remaputils:v<NEW-VERSION>
```

For example, if the version of the new release is `0.0.1`:

```sh
docker push registry.docker.decafhub.com/decaf-clever-runtime-remaputils:v0.0.1
```

> **NOTE:** To push to and pull from the Docker registry
> `registry.docker.decafhub.com`, you need to login at least once on your local
> computer where you want to push and pull Docker images:
>
> ```sh
> docker login registry.docker.decafhub.com
> ```
