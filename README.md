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

Docker images are hosted on a proprietary Docker image registry:

```txt
registry.docker.decafhub.com
```

You should first authenticate against this registry before pulling from or
pushing to it. This has to be done once on each host (workstation or server)
unless your authentication credentials have changed:

```sh
docker login registry.docker.decafhub.com
```

Then on an ongoing bases, you can pull the Docker image for a given version as
follows:

```sh
docker pull registry.docker.decafhub.com/decaf-clever-runtime-remaputils:v<VERSION>
```

In addition to official releases versioned as `v<VERSION>`, you can pull Docker
images built and pushed for preview purposes:

```sh
docker pull registry.docker.decafhub.com/decaf-clever-runtime-remaputils:pr-<GITHUB-PR-NUMBER>
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

> **Note**
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
for how you can do it.

### Preview Builds When Using GitHub Pull Requests

Each time there is new pull request or there is an update to the pull request, a
new Docker image is built and pushed for preview and testing purposes via a
GitHub action.

You can then pull these Docker images as follows:

```txt
docker pull registry.docker.decafhub.com/decaf-clever-runtime-remaputils:pr-<GITHUB-PR-NUMBER>
```

GitHub action will keep building and pushing Docker images as there are new
commits pushed to the PR.

However, GitHub action will keep using the same Docker image tag
(`pr-<GITHUB-PR-NUMBER>`) when building and pushing Docker images to the
registry.

This means that:

1. The Docker image on the registry with the same tag will be overridden.
2. You have to pull the new build again even if you have pulled one with the
   same tag before.

> **Warning**
>
> There is nothing preventing you from using these images on production, but we
> strongly advise against doing so as reproducibility is not guaranteed. Do so
> with caution only if there really is some urgency and it is documented and/or
> communicated with stakeholders.

### Production Builds After Release

Each time there is a new tag pushed to the Git repository, a GitHub action
builds a new Docker image with the tag as the version.

For example, if there is a new tag `v<VERSION>` pushed to the Git repository,
then the GitHub action will build and push a new Docker image that you can pull
as follows:

```sh
docker push registry.docker.decafhub.com/decaf-clever-runtime-remaputils:v<NEW-VERSION>
```

If, for some reason, you need to manually build and push the Docker image to the
registry, follow the instructions below.

Build the image (replace `<NEW-VERSION>` with the version of the release):

```sh
docker build --build-arg VERSION=<NEW-VERSION> --tag registry.docker.decafhub.com/decaf-clever-runtime-remaputils:v<NEW-VERSION> .
```

For example, if the version of the new release is `0.0.1`:

```sh
docker build --build-arg VERSION=0.0.1 --tag registry.docker.decafhub.com/decaf-clever-runtime-remaputils:v0.0.1 .
```

Then, push the successfully built Docker image to the registry so that others
can start using it in production, again, by replacing `<NEW-VERSION>` with the
version of the release:

```sh
docker push registry.docker.decafhub.com/decaf-clever-runtime-remaputils:v<NEW-VERSION>
```

For example, if the version of the new release is `0.0.1`:

```sh
docker push registry.docker.decafhub.com/decaf-clever-runtime-remaputils:v0.0.1
```
