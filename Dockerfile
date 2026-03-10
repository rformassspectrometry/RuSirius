FROM bioconductor/bioconductor_docker:RELEASE_3_22

LABEL name="rformassspectrometry/rusirius" \
      url="https://github.com/rformassspectrometry/RuSirius" \
      maintainer="philippine.louail@eurac.edu" \
      description="Docker container with RuSirius package and Sirius 6.3 for metabolite identification. Includes RStudio Server with all dependencies pre-installed. Sirius REST API starts automatically on port 9999." \
      license="Artistic-2.0"

WORKDIR /home/rstudio

## Copy package source
COPY --chown=rstudio:rstudio . /home/rstudio/

## Install CI tools and the current package (vignettes not built - require Sirius login)
RUN Rscript -e "install.packages(c('rcmdcheck', 'BiocCheck', 'sessioninfo'), repos = BiocManager::repositories())"
RUN Rscript -e "devtools::install('.', dependencies = TRUE, type = 'source', build_vignettes = FALSE, repos = BiocManager::repositories())"

## root user needed for rstudio server properly working
USER root

## Clean up
RUN rm -rf /tmp/*

## Install Sirius 6.3.3
RUN wget -nv https://github.com/sirius-ms/sirius/releases/download/v6.3.3/sirius-6.3.3-linux-x64.zip && \
    unzip sirius-*.zip && \
    rm sirius-*.zip && \
    chown -R rstudio:rstudio sirius && \
    ln -s /home/rstudio/sirius/bin/sirius /usr/local/bin/sirius && \
    echo "export PATH=/home/rstudio/sirius/bin:\$PATH" >> /home/rstudio/.bashrc

## Copy Sirius init script (starts Sirius REST API on port 9999 at container startup)
COPY ./scripts/sirius-init.sh /etc/cont-init.d/03_sirius
RUN chmod a+x /etc/cont-init.d/03_sirius
