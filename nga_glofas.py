from ochanticipy import (
    CodAB,
    GeoBoundingBox,
    create_custom_country_config,
    GlofasReforecast,
    GlofasForecast
)

from dateutil import rrule

leadtime_max = 7
country_config = create_custom_country_config(
    "/Users/caldwellst/Desktop/nga_niger_benue.yaml"
)
codab = CodAB(country_config=country_config)
# codab.download()

admin0 = codab.load(admin_level=0)
geo_bounding_box = GeoBoundingBox.from_shape(admin0)

glofas_reforecast = GlofasReforecast(
    country_config=country_config,
    geo_bounding_box=geo_bounding_box,
    leadtime_max=leadtime_max,
)
#glofas_reforecast.download()
#glofas_reforecast.process(clobber=True)
#ds_rf = glofas_reforecast.load()
glofas_forecast = GlofasForecast(
    country_config=country_config,
    geo_bounding_box=geo_bounding_box,
    leadtime_max=leadtime_max,
    end_date = "2022-09-01"
)
glofas_forecast.download()
glofas_forecast.process()

glofas_forecast = GlofasForecast(
    country_config=country_config,
    geo_bounding_box=geo_bounding_box,
    leadtime_max=leadtime_max,
    start_date = "2022-09-02",
    end_date = "2023-06-13"
)
glofas_forecast.download()
glofas_forecast.process()