FROM bioconductor/bioconductor_docker:3.20-R-4.4.2

LABEL name="rformassspectrometry/rusirius" \
      url="https://github.com/rformassspectrometry/RuSirius" \
      maintainer="philippine.louail@eurac.edu" \
      description="Docker container to run the vignette and checks for the RuSirius package. This version bases on the Bioconductor devel docker image." \
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

# Use Conda to install r-sirius-ms from conda-forge
RUN /opt/conda/bin/conda install -c conda-forge r-sirius-ms

# Add Sirius to PATH globally for RStudio
# RUN echo 'export PATH=/opt/conda/pkgs/sirius-ms-6.1.0-ha770c72_2/bin:${PATH}' > /.profile

# Set path 
RUN Rscript -e "Sys.setenv(PATH = paste('/opt/conda/pkgs/sirius-ms-6.1.0-ha770c72_2/bin', Sys.getenv('PATH'), sep = ':'))"

# Install the required R packages
RUN Rscript -e "BiocManager::install(c('sneumann/xcms', 'msdata'), ref = 'devel', ask = FALSE, dependencies = TRUE)"

# Install the current package with vignettes
RUN Rscript -e "devtools::install('.', dependencies = TRUE, type = 'source', build_vignettes = FALSE, repos = BiocManager::repositories())"
