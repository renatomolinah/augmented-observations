pro plot_ibtracs

  ; ——————————————————————————————————————————————
  ; IBTrACS CSV URL
  url = 'https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r01/access/csv/ibtracs.NA.list.v04r01.csv'

  ; ——————————————————————————————————————————————
  ; Read the CSV file into an IDL structure then spearate into variables
  ibtracs = read_csv(url, COUNT = n_records, HEADER = headers)
  header_index = where(headers eq 'USA_ATCF_ID')
  atcf = ibtracs.(header_index)
  header_index = where(headers eq 'LAT')
  lat = ibtracs.(header_index)
  header_index = where(headers eq 'LON')
  lon = ibtracs.(header_index)
  header_index = where(headers eq 'WMO_WIND')
  vmax = ibtracs.(header_index)
  header_index = where(headers eq 'NATURE')
  nature = ibtracs.(header_index)
  header_index = where(headers eq 'ISO_TIME')
  time = ibtracs.(header_index)
  header_index = where(headers eq 'IFLAG')
  flag = ibtracs.(header_index)

  ; ——————————————————————————————————————————————
  ; Create category rating based on wind speed
  categ=intarr(n_records)
  vmax=float(vmax)
  for i = 0, n_records-1 do begin
    if vmax[i] lt 34.                      then categ[i]=0  ;TD
    if vmax[i] ge 34.  and vmax[i] lt 64.  then categ[i]=1  ;TS
    if vmax[i] ge 64.  and vmax[i] lt 83.  then categ[i]=2  ;H1
    if vmax[i] ge 83.  and vmax[i] lt 96.  then categ[i]=3  ;H2
    if vmax[i] ge 96.  and vmax[i] lt 113. then categ[i]=4  ;H3
    if vmax[i] ge 113. and vmax[i] lt 137. then categ[i]=5  ;H4
    if vmax[i] ge 137.                     then categ[i]=6  ;H5
  endfor

  ; ——————————————————————————————————————————————
  ; Create a binary "nature" called "status"... 0 for non-TC and 1 for TC
  status=intarr(n_records)
  nature_index=where(nature eq 'SS' or nature eq 'TS',nature_count)
  if nature_count gt 0 then status[nature_index]=1

  ; ——————————————————————————————————————————————
  ; Select U.S. hurricane landfalls (2017-2020)
  selected = where(atcf eq 'AL092017' or atcf eq 'AL112017' or atcf eq 'AL062018' or atcf eq 'AL072018' or atcf eq 'AL142018' or $
                   atcf eq 'AL082020' or atcf eq 'AL092020' or atcf eq 'AL132020' or atcf eq 'AL192020' or atcf eq 'AL222020' and $
                   flag eq 'O______________',count)

  ; ——————————————————————————————————————————————
  ; Reassign variables to only include selected cases
  atcf = atcf[selected]
  lat = float(lat[selected])
  lon = float(lon[selected])
  categ = categ[selected]
  status = status[selected]
  time = time[selected]

  ; ——————————————————————————————————————————————
  ; Plot selected tracks on a map
  mapbound = [23,259,43,291]
  position = [0.05,0.05,0.95,0.93]
  m=map('Equirectangular',center_longitude=275,true_scale_latitude=0,limit=mapbound,$
    axis_style=2,dimensions=[1000,650],/box_axes,linestyle=6,label_position=0,$
    font_size=20,font_style='normal',position=position,title='Select U.S. Landfall Cases, 2017-2020')
  m['Latitudes'].label_angle=90
  m['Longitudes'].label_angle=0
  grid=m.mapgrid
  grid.grid_longitude=5
  grid.grid_latitude=5
  grid.font_size=10
  grid.font_style='normal'
  grid2=mapgrid(longitude_min=mapbound[1],longitude_max=mapbound[3],latitude_min=mapbound[0],latitude_max=mapbound[2],$
    grid_longitude=5,grid_latitude=5,/box_axes,label_position=1,font_size=10,linestyle=6)
  FOREACH g,grid2.latitudes DO g.label_angle=270
  FOREACH g,grid2.latitudes DO g.label_valign=-0.5
  FOREACH g,grid2.longitudes DO g.label_show=0
  c1=mapcontinents(/continents,/hires,fill_color='linen',linestyle=6)
  c2=mapcontinents(/usa,/hires,thick=0.3,color='tan')
  c3=mapcontinents(/countries,/hires,thick=0.5,color='tan')
  c4=mapcontinents(/continents,/hires,thick=0.7,color='tan')

  ; Count number of selected storms and loop over them, plotting each one
  index_cases = uniq(atcf)
  n_cases = n_elements(index_cases)
  for s = 0, n_cases-1 do begin
    storm = where(atcf eq (atcf[index_cases])[s],n_times)
    for t = 0, n_times-2 do begin
      if (status[storm])[t] eq 0 then linestyle=2 else linestyle=0  ;dashed line for non-TC, solid line for TC
      if (categ[storm])[t] eq 0 then color='turquoise'  ;TD
      if (categ[storm])[t] eq 1 then color='blue'       ;TS
      if (categ[storm])[t] eq 2 then color='lime green' ;H1
      if (categ[storm])[t] eq 3 then color='dark green' ;H2
      if (categ[storm])[t] eq 4 then color='dark orange';H3
      if (categ[storm])[t] eq 5 then color='red'        ;H4
      if (categ[storm])[t] eq 6 then color='maroon'     ;H5
      tracks = plot(lon[storm[t]:storm[t+1]],lat[storm[t]:storm[t+1]],color=color,linestyle=linestyle,thick=2,/overplot)  
    endfor
    ; Plot gray dots at each 00Z position
    newday = where(strmid(time[storm],11,5) eq '00:00',newday_cnt)
    if newday_cnt gt 0 then zero=symbol((lon[storm])[newday],(lat[storm])[newday],symbol='circle',sym_thick=1,sym_size=0.4,sym_color='gray',sym_filled=1,/data)

    ; Plot color-coded dot at U.S. landfall point
    ; For simplicity, these are manually defined for the ten selected cases
    ; For a larger dataset, create an automated coastline-crossing routine
    if (atcf[index_cases])[s] eq 'AL092017' then begin & lf_lat=28.0 & lf_lon=-96.9 & lf_color='red' & endif
    if (atcf[index_cases])[s] eq 'AL112017' then begin & lf_lat=24.7 & lf_lon=-81.5 & lf_color='red' & endif
    if (atcf[index_cases])[s] eq 'AL062018' then begin & lf_lat=34.2 & lf_lon=-77.8 & lf_color='lime green' & endif
    if (atcf[index_cases])[s] eq 'AL072018' then begin & lf_lat=30.4 & lf_lon=-88.5 & lf_color='blue' & endif
    if (atcf[index_cases])[s] eq 'AL142018' then begin & lf_lat=30.0 & lf_lon=-85.5 & lf_color='maroon' & endif
    if (atcf[index_cases])[s] eq 'AL082020' then begin & lf_lat=26.8 & lf_lon=-97.3 & lf_color='lime green' & endif
    if (atcf[index_cases])[s] eq 'AL092020' then begin & lf_lat=33.9 & lf_lon=-78.5 & lf_color='lime green' & endif
    if (atcf[index_cases])[s] eq 'AL132020' then begin & lf_lat=29.8 & lf_lon=-93.3 & lf_color='red' & endif
    if (atcf[index_cases])[s] eq 'AL192020' then begin & lf_lat=30.3 & lf_lon=-87.7 & lf_color='dark green' & endif
    if (atcf[index_cases])[s] eq 'AL222020' then begin & lf_lat=28.4 & lf_lon=-96.4 & lf_color='blue' & endif
    s1 = symbol(lf_lon,lf_lat,symbol='circle',sym_thick=3,sym_size=0.7,sym_color=lf_color,sym_filled=1,/data)
  endfor

  ; Plot a legend for the colored tracks
  t = text(0.07,0.88,'Tropical Depression', color='turquoise',  font_size=14,font_style='bold',/normal)
  t = text(0.07,0.85,'Tropical Storm',      color='blue',       font_size=14,font_style='bold',/normal)
  t = text(0.07,0.82,'Category 1 Hurricane',color='lime green', font_size=14,font_style='bold',/normal)
  t = text(0.07,0.79,'Category 2 Hurricane',color='dark green', font_size=14,font_style='bold',/normal)
  t = text(0.07,0.76,'Category 3 Hurricane',color='dark orange',font_size=14,font_style='bold',/normal)
  t = text(0.07,0.73,'Category 4 Hurricane',color='red',        font_size=14,font_style='bold',/normal)
  t = text(0.07,0.70,'Category 5 Hurricane',color='maroon',     font_size=14,font_style='bold',/normal)

  ; Save graphic as PNG file
  m.save,'landfall_cases_2017-2020.png',resolution=150

end