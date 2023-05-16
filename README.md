# R Library and DECAF Clever Runtime for REMAP Utilities

> **TODO:** Add introduction and usage sections.

## DECAF Clever Runtime

This repository provides DECAF Clever runtime for `remaputils`. It is built and
pushed to following Docker repository:

```txt
registry.docker.decafhub.com/decaf-clever-runtime-remaputils
```

### During Development

It is a good practice to test `remaputils` in its own runtime during
development.

Let's say that you made a change to `remaputils`. Now, you want to test it
whether it installs and works correctly in the runtime.

First, we need to build the rumtime, ie. build its Docker image:

```sh
docker build --build-arg VERSION=testing --tag runtime-remaputils-testing .
```

> **NOTES:**
>
> We are passing a `--build-arg` (Docker build argument), namely `VERSION`, that
> will set the runtime environment variable
> `DECAF_CLEVER_RUNTIME_IMAGE_VERSION`. Since we are not making a production
> release, we are setting it to `testing`.
>
> Also, we are setting the tag as `runtime-remaputils-testing` which is more or
> less an arbitrary Docker image name. You can choose something else, but then,
> you should use that name in the instructions below.

It may take a while. If you do not change dependencies (or `install.sh` and
`Dockerfile`), Docker will skip those steps which performs the bulk of the
installation.

Once ready, try the image:

```sh
docker run --rm runtime-remaputils-testing env
```

Indeed, you can simulate a DECAF Clever job. See
[./var/clever/examples/basic/README.md](./var/clever/examples/basic/README.md)
how you can do it.

You can use the same Docker image to `check` the R package, too:

```sh
docker run --rm -it -v "$(pwd):/tmp/remaputils" runtime-remaputils-testing R --quiet --vanilla -e 'devtools::check("/tmp/remaputils")'
```

### After Release

After you make a new release, you can issue the following command to rebuild the
runtime:

```sh
docker build --build-arg VERSION=<NEWVERSION> --tag registry.docker.decafhub.com/decaf-clever-runtime-remaputils:v<NEWVERSION> .
```

Then, you can push it to the Docker registry so that we can start using it in
production:

```sh
docker push registry.docker.decafhub.com/decaf-clever-runtime-remaputils:v<NEWVERSION>
```
