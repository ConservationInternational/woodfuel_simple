; This program prepares the climate input layers for fwp_growthdays_v01.pro
; It reads daily minimum and maximum temperature data and precipitation values
; representative for a landscape (from aWhere) from a text file and generates
; output files in GeoTIFF format.
;  
; Input data are:
; 1) Comma-delimited text file with four columns: Date, MaxTemp, MinTemp, Precip, MeanTemp.
; 2) Single-band GeoTIFF file of the exact dimensions the output files will be.
;
; Changes in version v02: output are min and max temperatures and precipitation only. 
;  

PRO fws_climprep_v02

  cd, 'C:\Fuelwood14\L1\awhere_2012'
  ;climfile = dialog_pickfile(title='Select Comma-Delimited INPUT File', filter='*.csv')
  ;IF climfile EQ '' THEN retall
  climfile = 'aWhere+2012_L1.csv'
  ;tmpl_file = dialog_pickfile(title='Select template INPUT File', filter='*.nvi')
  ;IF tmpl_file EQ '' THEN retall
  tmpl_file = 'Template.dat'
  
  ; Read ENVI template file
  print, 'Reading ENVI template file'
  envi_open_file, tmpl_file, r_fid=fid, /INVISIBLE, /NO_INTERACTIVE_QUERY, /NO_REALIZE
  IF (fid EQ -1) THEN return       ; Check, if opened file is valid, if not exit program
  envi_file_query, fid, ns=ns, nl=nl, nb=nb, bnames=bnames_fews, $
    data_type=data_type ; Read # samples, lines, bands from header
  map_info = envi_get_map_info(fid=fid)
  envi_file_mng, id=fid, /remove  
  print, '   ns = ', strtrim(ns, 2)
  print, '   nl = ', strtrim(nl, 2)
  tmpl = make_array(ns, nl, /float)
  openr, lun, climfile, /get_lun
  readu, lun, tmpl
  free_lun, lun 
  
  ; Read *.csv text file with climate parameters
  print, 'Reading text file with climate parameters' 
  openr, lun, climfile, /get_lun
  climtext = '' ; Define an empty string
  textline = '' ; Define temporary string variable
  WHILE ~ eof(lun) DO BEGIN ; Loop until end of file is found
    readf, lun, textline    ; Read a line of text
    climtext = [climtext, textline] ; Add to string array
  ENDWHILE
  climtext = climtext[2:n_elements(climtext)-1]  ; Remove empty first line and header line
  free_lun, lun
  nb = n_elements(climtext) ; Number of days for which data is available
  
  print, '    Number of days: ', strtrim(nb, 2)
  vals = strsplit(climtext[0], ',', /extract)
  firstday = vals[0]
  print, '    Start date: ', firstday
  vals = strsplit(climtext[nb-1], ',', /extract)
  lastday = vals[0]  
  print, '    End date: ', lastday
  
  band = make_array(ns, nl, /float)   ; Temporary data band
  temptrmin = make_array(ns, nl, nb, /float)   ; Minimum daily temperature
  temptrmax = make_array(ns, nl, nb, /float)   ; Maximum daily temperature  
  precip = make_array(ns, nl, nb, /float)      ; Mean daily precipitation
  bnames = make_array(nb, /string)             ; Band names to hold dates 
  
  print, 'Creating ENVI files of temperature and precipitation datasets'
  FOR b=0, nb-1 DO BEGIN
    vals = strsplit(climtext[b], ',', /extract)
    bnames[b] = vals[0]                ; Date in format yyyy-mm-dd    
    band[*] = float(vals[2])      ; Minimum temperature 
    temptrmin[*,*,b] = band
    band[*] = float(vals[1])      ; Maximum temperature 
    temptrmax[*,*,b] = band
    band[*] = float(vals[3])      ; Precipitation
    precip[*,*,b] = float(vals[3])    
  ENDFOR  

  ; Daily minimum temperatures
  outfname = (strsplit(climfile,'.',/extract))[0] + '_temptrmin_' + $
             firstday + '_' + lastday + '.nvi'
  print, 'Writing minimum daily temperatures to file ', outfname
  openw, lun, outfname, /get_lun
  writeu, lun, temptrmin
  free_lun, lun
  envi_setup_head, fname=outfname, ns=ns, nl=nl, nb=nb, data_type=4, $
    bnames=bnames, interleave=0, map_info=map_info, /write
    
  ; Daily maximum temperatures
  outfname = (strsplit(climfile,'.',/extract))[0] + '_temptrmax_' + $
             firstday + '_' + lastday + '.nvi'
  print, 'Writing minimum daily temperatures to file ', outfname
  openw, lun, outfname, /get_lun
  writeu, lun, temptrmax
  free_lun, lun
  envi_setup_head, fname=outfname, ns=ns, nl=nl, nb=nb, data_type=4, $
    bnames=bnames, interleave=0, map_info=map_info, /write

  ; Daily precipitation
  outfname = (strsplit(climfile,'.',/extract))[0] + '_precip_' + $
             firstday + '_' + lastday + '.nvi'
  print, 'Writing daily precipitation to file ', outfname
  openw, lun, outfname, /get_lun
  writeu, lun, precip
  free_lun, lun
  envi_setup_head, fname=outfname, ns=ns, nl=nl, nb=nb, data_type=4, $
    bnames=bnames, interleave=0, map_info=map_info, /write
 
  print, ' '
  print, 'Done, have a nice day! :)'
  print, ' '
 
END

    
  