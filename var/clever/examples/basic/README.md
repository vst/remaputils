# Basic DECAF Clever Job Example

This is a basic (and dummy) example for simulating a DECAF Clever job with your
own image and settings.

First, create a `config.yaml` file from the given `config.tmpl.yaml` template
(`config.yaml` is gitignored, so you can use real-world configuration):

```sh
cp `config.tmpl.yaml` `config.yaml`
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
