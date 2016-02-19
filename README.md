# ReSun & WRF output analysis
Creates .kmz (Google Earth) and .png with geographical data with the results of the ReSun & WRF simulations. Outputs graphs with median daily radiation for the simulated area.

This tool is used to have a critical analysis over the WRF & ReSun results, to study the potencial Solar Energy of that particular area.

Work in progress!!!

## Results:

**High quality image (png) example**

* Global horizontal radiation
![alt text](obs/Rad_2009-05-01.png)
![alt text](obs/Rad_2009-05-02.png)

**Google Earth (kmz) example**

* Global horizontal radiation
![alt text](obs/kmz.png)

* Map widget




**Animations (GIF) example**

* Global horizontal radiation
![alt text](obs/Rad_2009-05-25.gif)

**Graphics analysis**

* Daily median of studied period
![alt text](obs/Rad_daily_2009-04-30.png)


* Time Series of studied period
![alt text](obs/Rad_hour_TS_2009-04-30.png)

<iframe width="640" height="480" frameborder="0" seamless="seamless" scrolling="no" src="https://plot.ly/~ricardo88faria/2/radiacao-solar-diaria-na-ilha-da-madeira"></iframe>


* Hourly median of studied period
![alt text](obs/Rad_month_2009-04-30.png)

## Usage:

* Run:
```r
make run
```

* kill application:
```r
make kill
```

Contacts:

<ricardo88faria@gmail.com>
