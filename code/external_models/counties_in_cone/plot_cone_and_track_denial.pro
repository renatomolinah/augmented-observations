; ==============================================================================
; Denial cone plotter
; ------------------------------------------------------------------------------
; Plot the forecast cone and track geometry for one denial-case storm and write
; the derived shapefiles used downstream.
; ==============================================================================

pro plot_cone_and_track_denial,outdir,stormid,stormname,year,type,model,exper,fcst,fhr,lat,lon,wind


  ;history of cone sizes
  cone_yrs=['2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019','2020','2021','2022']
  cone_rad=[[   10,   49,   85,  121,  164,  232,  318,  439],$ ;2003
            [   10,   46,   81,  116,  153,  220,  292,  398],$ ;2004  *this year is interpolated between 2003 and 2005*
            [   10,   44,   77,  111,  143,  208,  266,  357],$ ;2005
            [   10,   40,   70,  100,  128,  188,  231,  303],$ ;2006
            [   10,   39,   69,   99,  124,  179,  252,  326],$ ;2007
            [   10,   39,   67,   92,  118,  170,  233,  305],$ ;2008
            [   10,   36,   62,   89,  111,  167,  230,  302],$ ;2009
            [   10,   36,   62,   85,  108,  161,  220,  285],$ ;2010
            [   10,   36,   59,   79,   98,  144,  190,  239],$ ;2011
            [   10,   36,   56,   75,   95,  141,  180,  236],$ ;2012
            [   10,   33,   52,   72,   92,  128,  177,  229],$ ;2013
            [   10,   33,   52,   72,   92,  125,  170,  226],$ ;2014
            [   10,   32,   52,   71,   90,  122,  170,  225],$ ;2015
            [   10,   30,   49,   66,   84,  115,  165,  237],$ ;2016
            [   10,   29,   45,   63,   78,  107,  159,  211],$ ;2017
            [   10,   26,   43,   56,   74,  103,  151,  198],$ ;2018
            [   10,   26,   41,   54,   68,  102,  151,  198],$ ;2019
            [   10,   26,   41,   55,   69,  103,  151,  196],$ ;2020
            [   10,   27,   40,   55,   69,  102,  148,  200],$ ;2021
            [   10,   26,   39,   52,   67,  100,  142,  200]]  ;2022
  cone_hrs= [    0,   12,   24,   36,   48,   72,   96,  120]
  
  
  z=where(cone_yrs eq year)

  ;read in county coordinates
  county_list=getenv('AO_COUNTY_LIST_FILE')
  if county_list eq '' then county_list='code/external_models/counties_in_cone/county_list.csv'
  n_counties=file_lines(county_list)-1
  header=''
  county_info=strarr(n_counties)
  openr,lun,county_list,/get_lun
  readf,lun,header
  readf,lun,county_info
  close,lun & free_lun,lun
  fips_id=strarr(n_counties)
  fips_county=strarr(n_counties)
  fips_state=strarr(n_counties)
  x_county_pts=fltarr(n_counties)
  y_county_pts=fltarr(n_counties)
  r_state=strarr(n_counties)
  r_county=strarr(n_counties)
  for c=0,n_counties-1 do begin
    tmp=strsplit(county_info[c],',',/extract)
    fips_id[c]=tmp[0]
    fips_county[c]=tmp[1]
    fips_state[c]=tmp[2]
    x_county_pts[c]=double(tmp[3])+360.0
    y_county_pts[c]=double(tmp[4])
    r_state[c]=tmp[5]
    r_county[c]=tmp[6]
  endfor
  
  
  ;plot cone
  ;position=[0.0,0.0,1.0,1.0]
  ;  i=image(sat_image,axis_style=0,dimensions=[956,528],position=position,/buffer)
  m=map('Mercator',limit=[23,255,50,295],dimensions=[900,700],position=[0.06,0.1,0.94,0.85],$
    title=strupcase(stormid)+year+' ('+strupcase(stormname)+')!C'+exper+' '+fcst,$
    /box_axes,label_position=0,font_size=18,font_style='bold')
  m.clip=0
  ;mg=mapgrid(thick=1,grid_latitude=1,grid_longitude=1,color='gray')
  
  grid=m.mapgrid
  grid.color='dark slate gray'
  grid.grid_longitude=gridspacing
  grid.grid_latitude=gridspacing
  grid.thick=0.1
  grid.font_size=9
  grid.clip=0
  grid.linestyle='dotted'
  m['Longitudes'].label_angle=0
  m['Latitudes'].label_angle=90
  ;create and draw cone
  delta=0.05    ;grid resolution for cone creation (degrees)
  timeres=0.5 ;interpolate track to this time resolution (hours)
  maxhr=max(float(fhr))     ;maximum time for cone (hours)
  cone_hrs2=findgen(maxhr/timeres+1)*timeres
  cone_rad2=spline(cone_hrs,cone_rad[*,z],cone_hrs2)
  lat2=spline(cone_hrs,lat,cone_hrs2)
  lon2=spline(cone_hrs,lon,cone_hrs2)
  x0=min(lon)-(max(cone_rad/50.0))
  x1=max(lon)+(max(cone_rad/50.0))
  y0=min(lat)-(max(cone_rad/50.0))
  y1=max(lat)+(max(cone_rad/50.0))
  xdim=ceil((x1-x0)/delta)
  ydim=ceil((y1-y0)/delta)
  lons_grid=findgen(xdim)*delta+x0
  lats_grid=findgen(ydim)*delta+y0
  xgrid=fltarr(xdim,ydim)
  ygrid=fltarr(xdim,ydim)
  rgrid=fltarr(xdim,ydim)
  static_grid=intarr(xdim,ydim)
  for ii=0,n_elements(cone_hrs2)-1 do begin
    for xx=0,xdim-1 do begin
      for yy=0,ydim-1 do begin
        xgrid[xx,yy]=(lons_grid[xx]-lon2[ii])*60.0*cos(lats_grid[yy]*!dtor)
        ygrid[xx,yy]=(lats_grid[yy]-lat2[ii])*60.0
        rgrid[xx,yy]=sqrt(xgrid[xx,yy]^2+ygrid[xx,yy]^2)
        if rgrid[xx,yy] le cone_rad2[ii] then static_grid[xx,yy]=1
      endfor
    endfor
  endfor
  p=plot(float(lon2),float(lat2),color='blue',thick=2,sym_size=0,linestyle=0,overplot=m)
  ;p=plot(float(lon),float(lat),color='green',thick=2,sym_size=0,linestyle=0,overplot=m)
  p=plot(float(lon),float(lat),color='blue',thick=2,symbol='circle',linestyle=6,overplot=m)
  c=contour(static_grid,lons_grid,lats_grid,c_value=1,    color='red',c_label_show=0,c_linestyle=0,c_thick=1,n_levels=1,overplot=m,grid_units=2)

  
  ;get coordinates of contour path (new function doesn't have this ability)
  ;https://www.l3harrisgeospatial.com/Support/Self-Help-Tools/Help-Articles/Help-Articles-Detail/ArtMID/10220/ArticleID/15959/Sneaky-way-to-get-contour-paths-from-CONTOUR-funtion
  set_plot,'z'
  contour,static_grid,lons_grid,lats_grid,levels=[1],path_xy=xy,path_info=xy_inf,/path_data_coords,/path_double,closed=0
  xy[*,0]=xy[*,-1]       ;make sure polygon is closed
  x_contour_pts=reform(xy[0,*])
  y_contour_pts=reform(xy[1,*])
  
  ;create shapefile of cone
  ;cone vertex coordinates
  file_mkdir,outdir,/noexpand_path
  xy_cone=xy
  xy_cone[0,*]=xy_cone[0,*]-360.0  ;set west longitude as negative
  shpfile = outdir+stormid+year+'_'+fcst+'_cone_'+type+'_denial_'+exper+'.shp'
  oshp = obj_new('IDLffShape',shpfile,/update,entity_type=5)
  oshp->AddAttribute,'Cone',7,25
  oshp->AddAttribute,'Vertices',3,8
  ent = {IDL_SHAPE_ENTITY}
  entity_index = 0
  ent.SHAPE_TYPE = 5
  ent.ISHAPE = entity_index
  ent.N_VERTICES = n_elements(x_contour_pts)
  ent.BOUNDS[0] = min(x_contour_pts)
  ent.BOUNDS[1] = min(y_contour_pts)
  ent.BOUNDS[2] = 0.0
  ent.BOUNDS[3] = 0.0
  ent.BOUNDS[4] = max(x_contour_pts)
  ent.BOUNDS[5] = max(y_contour_pts)
  ent.BOUNDS[6] = 0.0
  ent.BOUNDS[7] = 0.0
  ent.VERTICES = ptr_new(xy_cone);[x_contour_pts-360.0,y_contour_pts]
  attr = oshp->GetAttributes(/ATTRIBUTE_STRUCTURE)
  attr.ATTRIBUTE_0 = stormname+' '+fcst
  attr.ATTRIBUTE_1 = n_elements(x_contour_pts)
  oshp->PutEntity,ent
  ptr_free,ent.VERTICES
  oshp->SetAttributes,entity_index,attr
  obj_destroy,oshp
  
  ;spline-fit track vertex coordinates
  xy_trackline=transpose([[lon2-360.0],[lat2]])
  shpfile = outdir+stormid+year+'_'+fcst+'_trackline_'+type+'_denial_'+exper+'.shp'
  oshp = obj_new('IDLffShape',shpfile,/update,entity_type=3)
  oshp->AddAttribute,'Track',7,25
  oshp->AddAttribute,'Vertices',3,8
  ent = {IDL_SHAPE_ENTITY}
  entity_index = 0
  ent.SHAPE_TYPE = 3
  ent.ISHAPE = entity_index
  ent.N_VERTICES = n_elements(lon2)
  ent.BOUNDS[0] = min(lon2)
  ent.BOUNDS[1] = min(lat2)
  ent.BOUNDS[2] = 0.0
  ent.BOUNDS[3] = 0.0
  ent.BOUNDS[4] = max(lon2)
  ent.BOUNDS[5] = max(lat2)
  ent.BOUNDS[6] = 0.0
  ent.BOUNDS[7] = 0.0
  ent.VERTICES = ptr_new(xy_trackline);[x_contour_pts-360.0,y_contour_pts]
  attr = oshp->GetAttributes(/ATTRIBUTE_STRUCTURE)
  attr.ATTRIBUTE_0 = stormname+' '+fcst
  attr.ATTRIBUTE_1 = n_elements(lon2)
  oshp->PutEntity,ent
  ptr_free,ent.VERTICES
  oshp->SetAttributes,entity_index,attr
  obj_destroy,oshp

  ;forecast position coordinates
  xy_trackpoints=transpose([[lon-360.0],[lat]])
  shpfile = outdir+stormid+year+'_'+fcst+'_trackpoints_'+type+'_denial_'+exper+'.shp'
  oshp = obj_new('IDLffShape',shpfile,/update,entity_type=1)
  oshp->AddAttribute,'Track',7,25
  oshp->AddAttribute,'Forecast Hour',3,3
  oshp->AddAttribute,'Latitude',5,7,precision=2
  oshp->AddAttribute,'Longitude',5,7,precision=2
  oshp->AddAttribute,'Intensity (kt)',3,3
  for i=0,n_elements(lon)-1 do begin
    ent = {IDL_SHAPE_ENTITY}
    entity_index = i
    ent.SHAPE_TYPE = 1
    ent.ISHAPE = entity_index
    ent.N_VERTICES = 1
    ent.BOUNDS[0] = lon[i]-360.0
    ent.BOUNDS[1] = lat[i]
    ent.BOUNDS[2] = 0.0
    ent.BOUNDS[3] = 0.0
    ent.BOUNDS[4] = lon[i]-360.0
    ent.BOUNDS[5] = lat[i]
    ent.BOUNDS[6] = 0.0
    ent.BOUNDS[7] = 0.0
    ;ent.VERTICES = ptr_new(xy_trackpoints)
    attr = oshp->GetAttributes(/ATTRIBUTE_STRUCTURE)
    attr.ATTRIBUTE_0 = stormname+' '+fcst
    attr.ATTRIBUTE_1 = fhr[i]
    attr.ATTRIBUTE_2 = lat[i]
    attr.ATTRIBUTE_3 = lon[i]-360.0
    attr.ATTRIBUTE_4 = fix(wind[i])
    oshp->PutEntity,ent
    ;ptr_free,ent.VERTICES
    oshp->SetAttributes,entity_index,attr
  endfor
  obj_destroy,oshp


  ;determine which county points are inside that contour
  in_cone=inside(x_county_pts,y_county_pts,x_contour_pts,y_contour_pts)
  keep=where(in_cone eq 1)
  toss=where(in_cone eq 0)
  
  set_plot,'x'
  m.limit=[min(y_contour_pts)-2,min(x_contour_pts)-2,max(y_contour_pts)+2,max(x_contour_pts)+2]
  scat=scatterplot(x_county_pts[toss],y_county_pts[toss],symbol='dot',sym_color='light gray',sym_size=2,sym_filled=1,/overplot)
  scat=scatterplot(x_county_pts[keep],y_county_pts[keep],symbol='dot',sym_color='maroon',sym_size=2,sym_filled=1,/overplot)
  
  t=text(float(lon),float(lat),wind,color='dark blue',font_size=9,font_style='bold',/data)
  
  cont=mapcontinents(/usa,       /hires,thick=0.2,clip=0,noclip=1)
  cont=mapcontinents(/countries, /hires,thick=0.2,clip=0,noclip=1)
  cont=mapcontinents(/continents,/hires,thick=0.8,clip=0,noclip=1)
  
  ;write text file
  ;bring back original county_info data, but add binary cone check to end of each line
  outfile=outdir+stormid+year+'_'+fcst+'_countyconecheck_'+type+'_denial_'+exper+'.csv'
  openw,lun,outfile,width=150,/get_lun
  printf,lun,header,',in_cone'
  for c=0,n_counties-1 do begin
    printf,lun,county_info[c],',',string(in_cone[c],format='(i1)')
  endfor
  close,lun & free_lun,lun

end
