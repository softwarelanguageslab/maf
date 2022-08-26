Installation
==============

As per `REQUIREMENTS.md` a recent `x86_64` Linux machine is recommended with Docker installed.

## Running the Artifact

The artifact can be executed in two ways: by building it from source or by executing our pre-built Docker image.
The commands listed below will create a Docker image in the local Docker installation named `scam-scv-2022-artifact`. All commands need to be executed on the host where Docker has been installed.

### Building from Source

To build the artifact from source, an included `Dockerfile` can be used. This `Dockerfile` contains all the commands necessary to set-up a suitable building environment.
It can be executed using the following command: 

```
./scripts/artifact/build_from_source.sh 
```

### Running the existing Docker image (recommended)

In order to run the pre-built Docker image, it first needs to be imported in the Docker environment:

```
./scripts/artifact/import_image.sh
```

### Verifying if the Docker image has been correctly loaded

To quickly verify whether the Docker image has been correctly imported, execute the following command:

```
docker run -it scam-scv-2022-artifact repl
```

The above command will start e REPL (read-eval-print-loop) in which the artifact can be interactively tested. To test, input a valid Scheme expression, e.g. `(+ 1 1)`. 

Instructions for reproducing the tables in the paper are available in the `README.md` file.

