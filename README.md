# Landrace Gap Analysis

## Ojective
The overarching objetive is to develop a methodology which allows identifying geographical gaps according with the current conservation status at genebank centers across the world for 22 crops specifically for landraces. The purpose is to run a regional analysis depending on the center of origin for each crop.

## Input data
- Information discriminated by its genetic structure level
- Geographical coordinates
- Climate spatial data
- Socio-economic spatial data

## Methodology
- Identify spatial distribution of each genetic level
- Create a gap score according with geographical and environmental limitations

## Outputs
- A map with identified geographical gaps of each genetic level

# Study case: Common beans

## Input data
- Occurrences database: CIAT, USDA and GBIF databases with geographic coordinates of accessions
- Climate spatial data: Bioclim and Envirem variables
- Socio-economic spatial data.

## Methods
- Geographic and environmental constraints are calculated by: Kernel density, Cost distance, Delanuay triangulation, Environmental distance; using CIAT, USDA and GBIF (filtered by Germoplasm occurrences) databases.
- Spatial distribution is calculated using CIAT, USDA and GBIF (for all occurrences) databases.
