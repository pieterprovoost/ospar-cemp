# OSPAR CEMP assessment Shiny app

OSPAR is the mechanism by which 15 Governments and the European Union cooperate to protect the marine environment of the North-East Atlantic. OSPAR's [Coordinated Environmental Monitoring Programme (CEMP)](https://oap.ospar.org/en/ospar-monitoring-programmes/cemp/) aims to deliver comparable data from across the OSPAR Maritime Area. This Shiny application visualizes data collected under the CEMP on sediment contaminants in the North-East Atlantic.

Try the app at <https://shiny.pieterprovoost.be/ospar-cemp/>.

## How to run this app

- Download <https://ices-library.figshare.com/ndownloader/files/54859649> to `data/sediment_data.csv`
- Install renv with `install.packages("renv")`
- Activate the environment with `renv::restore()`
- Run with `shiny::runApp()`

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
