FROM sbtscala/scala-sbt:eclipse-temurin-17.0.3_1.7.1_3.1.3

# Copy the soure files to the docker build container
COPY . /build
WORKDIR /build
# Run the actual build of the source code
RUN sbt compile

# For running the processing scripts we will also need Python3, pandas and seaborn
RUN apt update && \
    apt install -y python3 python3-pip z3 libz3-java && \
    apt clean

# install tokei for the counting lines of code of the benchmarks
RUN apt-get update && apt-get install -y \
    curl \
    build-essential \
    git \
    pkg-config \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Install Rust and Cargo
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

# Install tokei
RUN cargo install tokei

# Install the required pip dependencies
RUN pip3 install seaborn pandas jinja2


