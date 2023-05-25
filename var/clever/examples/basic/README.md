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
    --config config.yaml \
    --tenant dev-sandbox \
    --runtime runtime-remaputils-testing \
    --params payload.json \
    --script script.R
```
