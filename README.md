# Covid19_Dashboard_Germany
Visit the App at [geobrinkmann.com](https://datageobrinkmann.be/Covid_DE_Dashboard/)

I am hosting the R-Shiny App on an [AWS EC2-Instance](https://aws.amazon.com/ec2). To keep the data up to date, I use [crontab](https://help.ubuntu.com/community/CronHowto) to execute the scrape_data.R script, daily.


## Credits
- Geocoding [D. Kisler](https://datascienceplus.com/osm-nominatim-with-r-getting-locations-geo-coordinates-by-its-address/)
- GPS locater via Dr. Tom August's [shiny geolocation Javascript script](https://github.com/AugustT/shiny_geolocation) 
- API calls to the current [COVID-19 Fallzahlen](https://services7.arcgis.com/mOBPykOjAyBO2ZKk/ArcGIS/rest/services/Covid19_RKI_Sums/FeatureServer/0/) und [ITS-Betten](https://www.divi.de/register/tagesreport) via [entorb's GitHub Seite](https://github.com/entorb/COVID-19-Coronavirus-German-Regions)


## Disclaimer
This tool is for entertainment purposes only and does not constitute medical, legal or any other form of advice. Users should refer to the official guidelines and recommendations of their national, state and local authorities. No personal data is stored.
