; This program calculates fuelwood scarcity adapted from R. Scholes'
; methodology used in the Millenium Ecosystem Assessment. The details
; are described in document "Protocol FWS v01a.doc", specifically
; Figure 2. 
; 
; The input data are (all at 1 km resolution):
; 1) Percent woody cover
; 2) Number of growth days in the current year (2010)
; 3) Human population density
; 

PRO fws_fuelwoodscarcity_v01

  cd, 'C:\Fuelwood14\L1\awhere_2012'
  pctwd_fname = 'TS_L1.dat'
  
  grdays_fname = 'GrowthDays_awhere+2012_L1.nvi'
  
  pop_fname = 'Popdens_L1.dat'
  fws_fname = 'FuelwoodScarcity_awhere+2012_L1_MAR.nvi'
    
  ; Read Input Files
  print, 'Reading percent woody cover file', pctwd_fname
  envi_open_file, pctwd_fname, /INVISIBLE, /NO_INTERACTIVE_QUERY, /NO_REALIZE, r_fid=fid
  IF (fid EQ -1) THEN return       ; Check, if opened file is valid, if not exit program
  envi_file_query, fid, ns=ns, nl=nl ; Read # samples, lines, bands from header
  map_info = envi_get_map_info(fid=fid)
  envi_file_mng, id=fid, /remove
  pctwd = make_array(ns, nl, /integer)
  openr, lun, pctwd_fname, /get_lun
  readu, lun, pctwd
  free_lun, lun
  pctwd = float(pctwd)
  
  print, 'Reading growth days file', grdays_fname
  envi_open_file, grdays_fname, /INVISIBLE, /NO_INTERACTIVE_QUERY, /NO_REALIZE, r_fid=fid
  IF (fid EQ -1) THEN return       ; Check, if opened file is valid, if not exit program
  envi_file_query, fid, ns=ns, nl=nl ; Read # samples, lines, bands from header
  envi_file_mng, id=fid, /remove
  grdays = make_array(ns, nl, /float)
  openr, lun, grdays_fname, /get_lun
  readu, lun, grdays
  free_lun, lun
  grdays = float(grdays)

  print, 'Reading population density file', pop_fname
  envi_open_file, pop_fname, /INVISIBLE, /NO_INTERACTIVE_QUERY, /NO_REALIZE, r_fid=fid
  IF (fid EQ -1) THEN return       ; Check, if opened file is valid, if not exit program
  envi_file_query, fid, ns=ns, nl=nl ; Read # samples, lines, bands from header
  envi_file_mng, id=fid, /remove
  pop = make_array(ns, nl, /integer)
  openr, lun, pop_fname, /get_lun
  readu, lun, pop
  free_lun, lun
  pop = float(pop)
 
  ; *** CALCULATE FUELWOOD SCARCITY ***
  print, 'Calculating Fuelwood Scarcity'
  
  ; Calculate Fuelwood Supply for each 1 x 1 km grid cell for 2010
  ; Methodology by Scholes et al. (2004): scale a maximum annual increment of 10.1 t/ha/yr (Ndayambaje et al., 2014)by
  ; a function of the number of days available for tree growth and the percentage tree cover.
  ; fuelwood supply = 10.1 (t/ha/yr) * 100 (ha,sqkm) * (woody cover % / 100) * growthdays (days/yr) / 365 (days/yr) =
  ; (t / sqkm / yr)
  ;fw_supply = 10.1 / 365 * grdays * (pctwd / 100)
  ;fw_supply = 10.1 * (pctwd / 100) * 100 / 365 * grdays
  fw_supply = 10.1 * 100 * (pctwd / 100) * (grdays / 365)
  
  ; Calculate overall fuelwood demand for each 1 x 1 km grid cell for 2012
  ; Drido and Nzabanita (2011) estimate fuel wood consumption in Rwanda
  ; in rural areas as 0.314 t / person / yr. 
  fw_demand = 0.314 * pop
  
  ; Distribute fuelwood among population iteratively and spatially explicit
  fw_scarcity = fw_supply - fw_demand
    ; If demand was higher than supply (i.e. fw_scracity < 0) in any one grid cell and there 
    ; is still supply left elsewhere (i.e. fw_scarcity > 0), distribute it
  IF (min(fw_scarcity) LT 0) AND (max(fw_scarcity) > 0) THEN BEGIN
    fw_dem = abs(total(fw_scarcity[where(fw_scarcity LT 0)]))   ; Total demand left
    fw_sup = abs(total(fw_scarcity[where(fw_scarcity GE 0)]))   ; Total supply left
    ; Distribute proportionally throughout the 10x10 km landscape, regardless of location 
    IF fw_sup GE fw_dem THEN BEGIN  ; If supply is greater than demand all demand can be satisfied
      fw_fact = 1 - (fw_dem / fw_sup)
      fw_scarcity[where(fw_scarcity GT 0)] = $
        fw_scarcity[where(fw_scarcity GT 0)] * fw_fact ; Reduce supply
      fw_scarcity[where(fw_scarcity LE 0)] = 0  ; Satisfy demand completely, set scarcity to zero
    ENDIF ELSE BEGIN  ; Else demand is greater than supply and *not* all demand can be satisfied
      fw_fact = 1 - (fw_sup / fw_dem)
      fw_scarcity[where(fw_scarcity LT 0)] = $
        fw_scarcity[where(fw_scarcity LT 0)] * fw_fact ; Reduce demand by distributing whatever supply is left
      fw_scarcity[where(fw_scarcity GT 0)] = 0  ; Reduce all supply to zero
    ENDELSE
  ENDIF
    
  ; Save result
  print, 'Writing fuelwood scarcity to file ', fws_fname
  openw, lun, fws_fname, /get_lun
  writeu, lun, fw_scarcity
  free_lun, lun
  envi_setup_head, fname=fws_fname, ns=ns, nl=nl, nb=1, data_type=4, $
    interleave=0, map_info=map_info, /write
  
  print, ' '
  print, 'Done, have a nice day! :)'
  print, ' '
 
END
