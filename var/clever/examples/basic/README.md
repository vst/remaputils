# Basic DECAF Clever Job Example

This directory provides is a basic (and quite dummy) example for simulating a
DECAF Clever job with your own runtime image and settings.

You need to create a `config.yaml` file from the given `config.tmpl.yaml`
template. Since `config.yaml` is gitignored, so you can safely use real-world
configuration.

First create the `config.yaml`:

```sh
cp config.tmpl.yaml config.yaml
```

Then, edit the `config.yaml` file as per your preference.

Finally, simulate a run:

```sh
decaf-clever job simulate \
    --runtime runtime-remaputils-testing \
    --config-repo-file config.yaml \
    --payload-file payload.json \
    --script-file script.R \
    --tenant dev-sandbox
```
