; This program calculates the number of growth days according to
; Linacre (1977) and Ellery et al. (1991). The results are used 
; to determine fuelwood scarcity. See Figure 1 in document 
; "Protocol FWS v01a.doc".
; The input data are
; 1) Daily temperature (one band per day since beginning of year)
; 2) Daily precipitation (one band per day since beginning of year)
; 3) Mean daily range of temperature (one band)
; 4) Altitude (one band)
; 5) Latitude (one band)
; 1 km resolution matching the respective 10 x 10 km landscape
; boundary. 

PRO fws_growthdays_v01

  firstyear = 2008    ; First year of long-term time series to calculate mean values
  lastyear = 2011     ; Last year of long-term time series to calculate mean values
  currentyear = 2012  ; Year for which number of growth days should be calculated

  cd, 'C:\Fuelwood14\L1\awhere_2012'
  temptrmin_fname = 'aWhere+2012_L1_temptrmin_2008-01-01_2012-12-31.nvi'
  temptrmax_fname = 'aWhere+2012_L1_temptrmax_2008-01-01_2012-12-31.nvi'
  precipdaily_fname = 'aWhere+2012_L1_precip_2008-01-01_2012-12-31.nvi'
  elevation_fname = 'elevation.dat' 
  latitude_fname = 'latitude.dat'
  growthdays_fname = 'GrowthDays_awhere+2012_L1.nvi'
  
  ; Read input files
  print, 'Reading daily minimum temperature file', temptrmin_fname
  envi_open_file, temptrmin_fname, /INVISIBLE, /NO_INTERACTIVE_QUERY, /NO_REALIZE, r_fid=fid
  IF (fid EQ -1) THEN return       ; Check, if opened file is valid, if not exit program
  envi_file_query, fid, ns=ns, nl=nl, nb=nb, bnames=bnames, $
    data_type=data_type ; Read # samples, lines, bands from header
  map_info = envi_get_map_info(fid=fid)
  envi_file_mng, id=fid, /remove
  temptrmin = make_array(ns, nl, nb, /float)
  openr, lun, temptrmin_fname, /get_lun
  readu, lun, temptrmin
  free_lun, lun
  
  print, 'Reading daily maximum temperature file', temptrmax_fname
  envi_open_file, temptrmax_fname, /INVISIBLE, /NO_INTERACTIVE_QUERY, /NO_REALIZE, r_fid=fid
  IF (fid EQ -1) THEN return       ; Check, if opened file is valid, if not exit program
  envi_file_query, fid, ns=ns, nl=nl, nb=nb, $
    data_type=data_type ; Read # samples, lines, bands from header
  envi_file_mng, id=fid, /remove
  temptrmax = make_array(ns, nl, nb, /float)
  openr, lun, temptrmax_fname, /get_lun
  readu, lun, temptrmax
  free_lun, lun
  
  print, 'Reading daily precipitation file', precipdaily_fname
  envi_open_file, precipdaily_fname, /INVISIBLE, /NO_INTERACTIVE_QUERY, /NO_REALIZE, r_fid=fid
  IF (fid EQ -1) THEN return       ; Check, if opened file is valid, if not exit program
  envi_file_query, fid, ns=ns, nl=nl, nb=nb, $
    data_type=data_type ; Read # samples, lines, bands from header
  envi_file_mng, id=fid, /remove
  precipdaily = make_array(ns, nl, nb, /float)
  openr, lun, precipdaily_fname, /get_lun
  readu, lun, precipdaily
  free_lun, lun
  
  print, 'Reading elevation file', elevation_fname
  envi_open_file, elevation_fname, /INVISIBLE, /NO_INTERACTIVE_QUERY, /NO_REALIZE, r_fid=fid
  IF (fid EQ -1) THEN return       ; Check, if opened file is valid, if not exit program
  envi_file_query, fid, ns=ns, nl=nl, $
    data_type=data_type ; Read # samples, lines, bands from header
  envi_file_mng, id=fid, /remove
  elevation = make_array(ns, nl, /integer)
  openr, lun, elevation_fname, /get_lun
  readu, lun, elevation
  free_lun, lun
  elevation = float(elevation)  
  
  print, 'Reading latitude file', latitude_fname
  envi_open_file, latitude_fname, /INVISIBLE, /NO_INTERACTIVE_QUERY, /NO_REALIZE, r_fid=fid
  IF (fid EQ -1) THEN return       ; Check, if opened file is valid, if not exit program
  envi_file_query, fid, ns=ns, nl=nl, $
    data_type=data_type ; Read # samples, lines, bands from header
  envi_file_mng, id=fid, /remove
  latitude = make_array(ns, nl, /float)
  openr, lun, latitude_fname, /get_lun
  readu, lun, latitude
  free_lun, lun
  
  ; *** START CALCULATING INPUT DATA SETS ***
 
  ; Adjust the daily minimum and maximum temperature, which is a coarse resolution average value to local topography
  elr = 6.49    ; Enviromental Lapse Rate, http://en.wikipedia.org/wiki/Lapse_Rate 6.49 K / 1000 m
  elev_avg = mean(elevation)   ;  Average elevation of the landscape
  FOR b=0, nb-1 DO BEGIN
    tmin_bd = temptrmin[*,*,b]
    tmax_bd = temptrmax[*,*,b]
    FOR s=0, ns-1 DO BEGIN
      FOR l=0, nl-1 DO BEGIN
        tmin_bd[s,l] = tmin_bd[s,l] + (elr/1000 * (elev_avg - elevation[s,l]))
        tmax_bd[s,l] = tmax_bd[s,l] + (elr/1000 * (elev_avg - elevation[s,l]))
        temptrmin[b] = tmin_bd
        temptrmax[b] = tmax_bd
      ENDFOR
    ENDFOR
  ENDFOR
  
  ; Calculate the mean daily range of temperature based on the time series  
  b = -1    ; Find start of first year of long-term time series
  REPEAT BEGIN
    b += 1
  ENDREP UNTIL fix(strmid(bnames[b],0,4)) EQ firstyear
  temptrrange = make_array(ns, nl, value=0.0, /float)
  bcnt = 0       ; Counter for number of bands over which values are averaged
  REPEAT BEGIN   ; Calculate average temperature range
    tmin_bd = temptrmin[b]
    tmax_bd = temptrmax[b]
    temptrrange = temptrrange + (tmax_bd - tmin_bd)   ; Add up all the ranges
    b += 1
    bcnt += 1
  ENDREP UNTIL fix(strmid(bnames[b],0,4)) GT lastyear
  temptrrange = temptrrange / bcnt    ; Calculate the long-term average
  
  ; Calculate monthly averages and the difference between the mean temperatures
  ; of the hottest and coldest months.  
  b = -1    ; Find start of first year of long-term time series
  REPEAT BEGIN
    b += 1
  ENDREP UNTIL fix(strmid(bnames[b],0,4)) EQ firstyear
  tmean_bd = make_array(ns, nl, /float, value=0.0)
  temptrmthly = make_array(ns, nl, 12, /float, value=0.0)
  daysmonth = make_array(12, /integer, value=0)   ; Days of each month  
  REPEAT BEGIN   ; Calculate average monthly temperatures
    tmin_bd = temptrmin[b]    ; Daily minimum temperature
    tmax_bd = temptrmax[b]    ; Daily maximum temperature
    tmean_bd = (tmin_bd + tmax_bd) / 2   ; Calculate daily mean temperature
    mth = fix(strmid(bnames[b],5,2))-1   ; Month 1 to 12
    temptrmthly[*,*,mth] = temptrmthly[*,*,mth] + tmean_bd
    daysmonth[mth] += 1    ; Keep track of the number of days per month (in case of missing data)
    b += 1
  ENDREP UNTIL fix(strmid(bnames[b],0,4)) GT lastyear
  FOR mth=0, 11 DO BEGIN    ; Calculate monthly average temperature
    temptrmthly[*,*,mth] = temptrmthly[*,*,mth] / daysmonth[mth]
  ENDFOR
  tmin_mth = 100    ; To hold minimum average monthly temperature
  tmax_mth = -100     ; To hold maximum average monthly temperature
  FOR mth=0, 11 DO BEGIN
    tmin_mth = min([tmin_mth,mean(temptrmthly[*,*,mth])])
    tmax_mth = max([tmax_mth,mean(temptrmthly[*,*,mth])])
  ENDFOR
  temptrmthrange = tmax_mth - tmin_mth   ; Diff. b/w mean temptr of hottest and coldest month
  
  ; Calculate (T - Td) as:
  ; (T - Td) = 0.0023 h + 0.37 T + 0.53 R + 0.35 R_ann - 10.9 degC
  ; where h is the elevation above sea level, T is the mean temperature,
  ;  R is the mean daily range of temperature, and R_ann is the 
  ; difference between the mean temperatures of the hottest and coldest months.
  ; Calculate values of the entire time series
  b = 0
  TTd = make_array(ns, nl, nb, /float, value=0.0)
  end_loop = 0    ; Boolean variable indicating end of the repeat until loop
  REPEAT BEGIN   ; Calculate (T - Td)
    temptrmean = (temptrmin[b] + temptrmax[b]) / 2
    TTd[*,*,b] = 0.0023*elevation + 0.37*temptrmean + 0.53*temptrrange + 0.35*temptrmthrange - 10.9
    b += 1
    IF (b EQ nb) THEN BEGIN   ; Evaluate whether to end the loop
      end_loop = 1
    ENDIF ELSE BEGIN
      IF (fix(strmid(bnames[b],0,4)) GT currentyear) THEN BEGIN
        end_loop = 1
      ENDIF
    ENDELSE
  ENDREP UNTIL end_loop
  
  ; Calculate Daily Potential Evapotranspiration E0
  ; E0 = (700*Tm/(100-A) + 15*(T-Td)) / (80-T)
  ; were Tm = T + 0.006 h with h being the elevation above sea level (meters),
  ; T is the mean temperature, A is the latitude (degrees), and Td is the mean dew point.
  ; (T-Td) has already been calculated above.
  ; Calculate values of the entire time series  
  evapotr = make_array(ns, nl, nb, /float, value=0.0)
  b = 0  
  end_loop = 0    ; Boolean variable indicating end of the repeat until loop
  REPEAT BEGIN   ; Calculate daily potential evapotranspiration
    temptrmean = (temptrmin[b] + temptrmax[b]) / 2
    TTd_bd = TTd[*,*,b]
    evapotr_bd = (700*(temptrmean + 0.006*elevation) / (100 - latitude) + 15*TTd_bd) / $
                 (80 - temptrmean)
    evapotr[*,*,b] = evapotr_bd
    b += 1
    IF (b EQ nb) THEN BEGIN   ; Evaluate whether to end the loop
      end_loop = 1
    ENDIF ELSE BEGIN
      IF (fix(strmid(bnames[b],0,4)) GT currentyear) THEN BEGIN
        end_loop = 1
      ENDIF
    ENDELSE
  ENDREP UNTIL end_loop
  
  ; *** END CALCULATING INPUT DATA SETS ***
  
  ; *** CALCULATE GROWTH DAYS ***
  nbc = n_elements(evapotr[0,0,*])   ; Number of bands (days) in the current year    
  sws = make_array(ns, nl, /float, value=0.0)  ; Soil Water Storage, initialize to zero  
  year = 0    ; Year being processed, initialize to zero
  b = 0
  end_loop = 0    ; Boolean variable indicating end of the repeat-until loop
  REPEAT BEGIN
    year = fix(strmid(bnames[b],0,4))
    print, 'Processing year ', strtrim(year,2)
    growthdays = make_array(ns, nl, /float, value=0.0)
    end_innerloop = 0
    plotvals_days = [0]
    plotvals_precip = [0]
    plotvals_sws = [0]
    plotvals_grdays = [0]    
    daycount = 0
    REPEAT BEGIN
      precip = precipdaily[*,*,b]
      sws += precip    ; Add daily precipitation to soil water storage
      evapotr_bd = evapotr[*,*,b]
      FOR s=0, ns-1 DO BEGIN
        FOR l=0, nl-1 DO BEGIN
          IF evapotr_bd[s,l] LT sws[s,l] THEN BEGIN
            fraction = 1.0
            sws[s,l] = sws[s,l] - evapotr_bd[s,l]
          ENDIF ELSE BEGIN
            fraction = sws[s,l] / evapotr_bd[s,l]
            sws[s,l] = 0
          ENDELSE
          growthdays[s,l] = growthdays[s,l] + fraction
        ENDFOR
      ENDFOR
      plotvals_days = [plotvals_days, daycount]
      plotvals_precip = [plotvals_precip, mean(precip)]
      plotvals_sws = [plotvals_sws, mean(sws)]
      plotvals_grdays = [plotvals_grdays, mean(growthdays)]
      ;print, 'evapotr = ', strmid(mean(evapotr_bd),2), '; sws = ', strmid(mean(sws),2), '; fraction = ', strmid(mean(fraction),2)
      daycount += 1
      b += 1
      IF (b EQ nb) THEN BEGIN   ; Evaluate whether to end the loop
        end_innerloop = 1
      ENDIF ELSE BEGIN
        IF (fix(strmid(bnames[b],0,4)) GT year) THEN BEGIN
          end_innerloop = 1
        ENDIF
      ENDELSE
    ENDREP UNTIL end_innerloop
    ;ymax = fix(max([max(plotvals_grdays),max(plotvals_precip),max(plotvals_sws)])*1.05)
    ymax = 150
    iPlot, [0], [0], xrange=[0,365], yrange=[0,ymax], title=strmid(year,2), use_default_color=0
    iPlot, plotvals_days, plotvals_grdays, color=([0,255,0]), name="GrDays", /overplot, /insert_legend
    iPlot, plotvals_days, plotvals_precip, color=([0,0,255]), name="Precip", /overplot, /insert_legend
    iPlot, plotvals_days, plotvals_sws, color=([255,0,0]), name="SWS", /overplot, /insert_legend
    IF (b EQ nb) THEN BEGIN   ; Evaluate whether to end the loop
      end_loop = 1
    ENDIF ELSE BEGIN
      IF (fix(strmid(bnames[b],0,4)) GT currentyear) THEN BEGIN
        end_loop = 1
      ENDIF
    ENDELSE
  ENDREP UNTIL end_loop
  
  ; Save result
  print, 'Writing growth days to file ', growthdays_fname
  openw, lun, growthdays_fname, /get_lun
  writeu, lun, growthdays
  free_lun, lun
  envi_setup_head, fname=growthdays_fname, ns=ns, nl=nl, nb=1, data_type=4, $
    interleave=0, map_info=map_info, /write
  
  print, ' '
  print, 'Done, have a nice day! :)'
  print, ' '
 
END
