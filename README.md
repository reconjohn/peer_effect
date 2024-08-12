# Code for: "Peer effects in the adoption of rooftop solar, EVs, and heat pumps across urban and rural areas in the United States"

The following includes descriptions of data, code, and functions used for data analyses and visualizations for the paper titled, "Peer effects in the adoption of rooftop solar, EVs, and heat pumps across urban and rural areas in the United States."



### Data Descriptions

1. **`tr_sf`**
   - *Description:* This object contains geospatial data at the tract level, identified by GEOID from 2019 American Community Survey (ACS).

2. **`PV`**
   - *Description:* This object contains rooftop solar adoption in Seattle from the city's open data portal. 

2. **`EV`**
   - *Description:* This object contains EV adoption in Seattle from the city's open data portal. 

2. **`Pump`**
   - *Description:* This object contains heat pump adoption in Seattle from the city's open data portal. 

2. **`VPV`**
   - *Description:* This object contains rooftop solar adoption in Vermont from Vermont Energy Dashboard of Energy Action Network (EAN). 

3. **`seattle_tracts`**
   - *Description:* This dataset provides socioeconomic and demogrphic variables in Seattle from the 2015-2019 American Community Survey (ACS). 

3. **`temp`**
   - *Description:* This dataset provides socioeconomic and demogrphic variables in Vermont from the 2015-2019 American Community Survey (ACS). 

4. **`s_house`**
   - *Description:* This data file contains building footprints for Seattle, collected from Microsoft USBuildingFootprints. 

4. **`v_house`**
   - *Description:* This data file contains Building footprints for Vermont, collected from Microsoft USBuildingFootprints. 

2. **`dfPV`**
   - *Description:* This object contains neighbor installations of adopters and non-adopters by distance and year for rooftop solar in Seattle.

2. **`dfEV`**
   - *Description:* This object contains neighbor installations of adopters and non-adopters by distance and year for EVs in Seattle.

2. **`dfPump`**
   - *Description:* This object contains neighbor installations of adopters and non-adopters by distance and year for heat pumps in Seattle.

2. **`dfVT`**
   - *Description:* This object contains neighbor installations of adopters and non-adopters by distance and year for rooftop solar in Vermont.

12. **`DAC_s.csv`**
      - *Description:* This data file includes disadvantaged community classification (CEJST), and DAC scores (Energy Justice Mapping Tool).

### Script Descriptions

1. **`Lib.R`**
   - *Description:* This file serves as a repository for essential libraries required for analysis, ensuring necessary resources are readily available for analytical processes.

2. **`Data.R`**
   - *Description:* This file serves as a repository for essential data required for analysis, ensuring necessary resources are readily available for analytical processes.

2. **`Diffusion_plot.R`**
   - *Description:* This script executes the code for generating visual representations (figures) for the manuscript and performing analyses. It plays a crucial role in translating data and research methods into meaningful insights and results.

