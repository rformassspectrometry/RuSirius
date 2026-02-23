FROM bioconductor/bioconductor_docker:RELEASE_3_22

LABEL name="rformassspectrometry/rusirius" \
      url="https://github.com/rformassspectrometry/RuSirius" \
      maintainer="philippine.louail@eurac.edu" \
      description="Docker container with RuSirius package environment. Note: Vignettes require Sirius login and cannot be run automatically." \
      license="Artistic-2.0"

WORKDIR /home/rstudio

# Copy the current directory to the container
COPY --chown=rstudio:rstudio . /home/rstudio/

# Install Miniconda
RUN apt-get update && apt-get install -y curl && \
    curl -fsSL https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -o miniconda.sh && \
    bash miniconda.sh -b -p /opt/conda && \
    rm miniconda.sh && \
    /opt/conda/bin/conda clean --all -y && \
    ln -s /opt/conda/etc/profile.d/conda.sh /etc/profile.d/conda.sh && \
    echo ". /opt/conda/etc/profile.d/conda.sh" >> /etc/profile && \
    echo "conda activate base" >> ~/.bashrc

# Accept conda ToS and configure to use only conda-forge
RUN /opt/conda/bin/conda config --remove channels defaults || true && \
    /opt/conda/bin/conda config --add channels conda-forge && \
    /opt/conda/bin/conda config --set channel_priority strict

# Use Conda to install r-sirius-ms (includes Sirius 6.3) from conda-forge only
RUN /opt/conda/bin/conda install --override-channels -c conda-forge r-sirius-ms -y

# Find and set Sirius path dynamically
RUN SIRIUS_PATH=$(find /opt/conda/pkgs -maxdepth 1 -type d -name "sirius-ms-*" | head -1) && \
    echo "export PATH=${SIRIUS_PATH}/bin:\${PATH}" >> /etc/profile.d/sirius.sh && \
    echo "export PATH=${SIRIUS_PATH}/bin:\${PATH}" >> /home/rstudio/.bashrc


# Install CI tools and the current package (vignettes not built - require Sirius login)
RUN Rscript -e "install.packages(c('rcmdcheck', 'BiocCheck', 'sessioninfo'), repos = BiocManager::repositories())"
RUN Rscript -e "devtools::install('.', dependencies = TRUE, type = 'source', build_vignettes = FALSE, repos = BiocManager::repositories())"

# Note: To run vignettes interactively, start the container and log in to Sirius:
# 1. docker run -it -p 8787:8787 rformassspectrometry/rusirius
# 2. Open RStudio at http://localhost:8787
# 3. In R: srs <- Sirius(username = "your_email", password = "your_password")
