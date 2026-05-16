! ==============================================================================
! P-RAIN input creator
! ------------------------------------------------------------------------------
! Create the 12-column storm_data_xxxx.txt files that PHRAM expects as input.
! The defaults now point at repo-native data locations, with environment
! variables available for storm selection and path overrides.
! ==============================================================================

program create_storm_data

  use utils
  implicit none

  character(len=4)      :: atcf_id, ships_year
  character(len=6)      :: ships_id, mc_time
  character(len=8)      :: ships_time, mc_id
  character(len=10)     :: start_time
  character(len=12)     :: my_fmt
  character(len=160)    :: ships_dir
  character(len=240)    :: ships_file, mc_file, out_file
  real                  :: col1, col2, col3, col4, col5
  real,parameter        :: dtor=3.141592654/180.0
  real,dimension(:),allocatable :: row1, row2, row3, uu, vv
  real,dimension(:),allocatable :: times, ships_hour, shr_mag, shr_dir, mc_hour, &
                                   vmax, lat, lon, rmw, uu_interp, vv_interp
  integer               :: i, ii, e, fhr_start, fhr_end, fhr_end_orig, bdeck_end, &
                           deltat, ntimes, nmembers, ships_ntimes
  logical               :: ships_found, skip_ships_download
  character(len=240)    :: line
  integer               :: n,a,b,c
  character(len=240)    :: output_dir, env_value, mc_input_dir, ships_cache_dir

  atcf_id = "al14"
  call get_environment_variable("AO_PRAIN_ATCF_ID", env_value)
  if (len_trim(env_value) > 0) atcf_id = trim(env_value)

  start_time = "2024100700"  !YYYYMMDDHH
  call get_environment_variable("AO_PRAIN_START_TIME", env_value)
  if (len_trim(env_value) > 0) start_time = trim(env_value)
  print*,trim(atcf_id),' ',trim(start_time)

  bdeck_end = 120 !If B-deck entries end before fhr_end, set that here (manual check)
  call get_environment_variable("AO_PRAIN_BDECK_END", env_value)
  if (len_trim(env_value) > 0) read(env_value,*) bdeck_end

  output_dir = "../../../../data/raw/prain/DATA/"//trim(atcf_id)//"_"//trim(start_time)//"/"
  call get_environment_variable("AO_PRAIN_DATA_OUTPUT_DIR", env_value)
  if (len_trim(env_value) > 0) output_dir = trim(env_value)
  if (output_dir(len_trim(output_dir):len_trim(output_dir)) .ne. '/') output_dir = trim(output_dir)//'/'
  call execute_command_line("mkdir -p "//trim(output_dir))

  mc_input_dir = "./"
  call get_environment_variable("AO_PRAIN_MC_INPUT_DIR", env_value)
  if (len_trim(env_value) > 0) mc_input_dir = trim(env_value)
  if (mc_input_dir(len_trim(mc_input_dir):len_trim(mc_input_dir)) .ne. '/') mc_input_dir = trim(mc_input_dir)//'/'

  !Read in mcrall file header to get fhr_start, fhr_end, and nmembers
  mc_id = atcf_id//start_time(1:4)   !al142018
  mc_time = start_time(5:10)         !100800
  mc_file = trim(mc_input_dir)//"mcrall_"//mc_id//"_"//mc_time//".dat"
  open(1,file=mc_file,form="formatted",status="old",action="read")
  read(1,*)
  read(1,fmt="(3(f6.1),f7.0)") col1,col2,col3,col4
  fhr_start=int(col1)
  fhr_end_orig=int(col2)
  nmembers=int(col4)
  close(1)
  print*,"nmembers: ",nmembers
  print*,"fhr_start: ",fhr_start
  print*,"fhr_end: ",fhr_end_orig


  !make an array of times (e.g. 0,6,...,120)
  if (bdeck_end .lt. fhr_end_orig) then 
    fhr_end=bdeck_end
  else
    fhr_end=fhr_end_orig
  end if
  print*,"updated fhr_end: ",fhr_end
  deltat = 6
  ntimes = (fhr_end-fhr_start)/deltat+1
  allocate(times(ntimes))
  do i=1,ntimes
    times(i) = (i-1)*deltat*1.0
  end do


  !only need the SHIPS file for the shear magnitude and direction 
  !e.g. ftp://ftp.nhc.noaa.gov/atcf/archive/MESSAGES/2018/stext/18100800AL1418_ships.txt
  ships_dir = "ftp://ftp.nhc.noaa.gov/atcf/archive/MESSAGES/"
  call get_environment_variable("AO_PRAIN_SHIPS_BASE_URL", env_value)
  if (len_trim(env_value) > 0) ships_dir = trim(env_value)
  if (ships_dir(len_trim(ships_dir):len_trim(ships_dir)) .ne. '/') ships_dir = trim(ships_dir)//'/'
  ships_cache_dir = "../../../../data/raw/prain/ships/"
  call get_environment_variable("AO_PRAIN_SHIPS_CACHE_DIR", env_value)
  if (len_trim(env_value) > 0) ships_cache_dir = trim(env_value)
  if (ships_cache_dir(len_trim(ships_cache_dir):len_trim(ships_cache_dir)) .ne. '/') ships_cache_dir = trim(ships_cache_dir)//'/'
  call execute_command_line("mkdir -p "//trim(ships_cache_dir))
  ships_id = Upper(atcf_id)//start_time(3:4)  !AL1418
  ships_year = start_time(1:4)                !2018
  ships_time = start_time(3:10)               !18100800
  ships_file = trim(ships_cache_dir)//trim(ships_time//ships_id//"_ships.txt")
  skip_ships_download = .false.
  call get_environment_variable("AO_PRAIN_SKIP_SHIPS_DOWNLOAD", env_value)
  if (len_trim(env_value) > 0) then
    if (trim(env_value) .eq. "1" .or. trim(env_value) .eq. "true" .or. trim(env_value) .eq. "TRUE" .or. &
        trim(env_value) .eq. "yes" .or. trim(env_value) .eq. "YES") skip_ships_download = .true.
  end if
  !print *, ships_file
  !download from NHC FTP server if needed
  inquire(file=ships_file,exist=ships_found)
  if ( .not. ships_found .and. .not. skip_ships_download ) then
    print*,"SHIPS file not found, trying download from NHC"
    call execute_command_line("wget -q -O "//trim(ships_file)//" "//trim(ships_dir)//ships_year//"/stext/"//trim(ships_time//ships_id//"_ships.txt"))
  end if
  !some years (like 2020) are kept in a strange subdirectory
  inquire(file=ships_file,exist=ships_found)
  if ( .not. ships_found .and. .not. skip_ships_download ) then
    print*,"SHIPS file not found, trying download from NHC"
    call execute_command_line("wget -q -O "//trim(ships_file)//" "//trim(ships_dir)//ships_year//"/stext/"//ships_year//"/"//trim(ships_time//ships_id//"_ships.txt"))
    inquire(file=ships_file,exist=ships_found)
  end if

  if (ships_found) then
    open(1,file=ships_file,form="formatted",status="old",action="read")
    print*,"SHIPS file found: ",ships_file

    !row1:TIME (HR), row2:SHEAR (KT), row3:SHEAR DIR
    if (fhr_end.eq.6) ships_ntimes=2
    if (fhr_end.eq.12) ships_ntimes=3
    if (fhr_end.eq.18) ships_ntimes=4
    if (fhr_end.eq.24) ships_ntimes=5
    if (fhr_end.eq.36) ships_ntimes=6
    if (fhr_end.eq.48) ships_ntimes=7
    if (fhr_end.eq.60) ships_ntimes=8
    if (fhr_end.eq.72) ships_ntimes=9
    if (fhr_end.eq.84) ships_ntimes=10
    if (fhr_end.eq.96) ships_ntimes=11
    if (fhr_end.eq.108) ships_ntimes=12
    if (fhr_end.eq.120) ships_ntimes=13
    allocate(row1(ships_ntimes))
    allocate(row2(ships_ntimes))
    allocate(row3(ships_ntimes))
    allocate(uu(ships_ntimes))
    allocate(vv(ships_ntimes))
    my_fmt = '(14x,13f6.0)'
    write(my_fmt,'(a,i0,a)') '(14x,',ships_ntimes,'f6.0)'

    do while(.true.)
      read(1,'(a)',end=99) line
      n=len_trim(line)
      a=index(line(1:n),'TIME (HR')
      if (a.eq.1) then
        !read(line,fmt="(14x,13(f6.0))") row1
        read(line,fmt=my_fmt) row1
      end if
      b=index(line(1:n),'SHEAR (KT')
      if (b.eq.1) then
        !read(line,fmt="(14x,13(f6.0))") row2
        read(line,fmt=my_fmt) row2
      end if
      c=index(line(1:n),'SHEAR DIR')
      if (c.eq.1) then
        !read(line,fmt="(14x,13(f6.0))") row3
        read(line,fmt=my_fmt) row3
      end if
    end do
99 continue

!**can't use strict formats because SHIPS file changes over time
!  do i=1,4
!    read(1,*)
!  end do
!  read(1,fmt="(14x,13(f6.0))") row1
!  do i=1,5
!    read(1,*)
!  end do
!  read(1,fmt="(14x,13(f6.0))") row2
!  read(1,*)
!  read(1,fmt="(14x,13(f6.0))") row3

    close(1)
  end if

  !now interpolate to deltat
  allocate(ships_hour(ntimes))
  allocate(shr_mag(ntimes))
  allocate(uu_interp(ntimes))
  allocate(vv_interp(ntimes))
  allocate(shr_dir(ntimes))
  
  if (ships_found) then
    call interp1(13,row1,row1,ntimes,times,ships_hour)
    call interp1(13,row1,row2,ntimes,times,shr_mag)
    !interpolating wind direction is trickier (convert to u,v, interpolate, convert to dir)
    uu = row2*sin((270.0+180.0-row3)*dtor)
    vv = row2*cos((270.0+180.0-row3)*dtor)
    call interp1(13,row1,uu,ntimes,times,uu_interp)
    call interp1(13,row1,vv,ntimes,times,vv_interp)
    shr_dir = mod((atan2(vv_interp,uu_interp)/dtor)+360.0,360.0)
  else
    print*,"SHIPS file not found, setting SHR_MAG=0 and SHR_DIR=0"
    shr_mag(:)=0.0
    shr_dir(:)=0.0
  end if

  !the Monte-Carlo ensemble file has 1-hourly values of lat, lon, vmax, rmw
  !want 6-hourly values, so read in every 6th line
 !  nmembers = 1000
 !  mc_id = atcf_id//start_time(1:4)   !al142018
 !  mc_time = start_time(5:10)         !100800
 !  mc_file = trim("mcrall_"//mc_id//"_"//mc_time//".dat")
  !print *, mc_file
  allocate(mc_hour(ntimes))
  allocate(vmax(ntimes))
  allocate(lat(ntimes))
  allocate(lon(ntimes))
  allocate(rmw(ntimes))
  open(1,file=mc_file,form="formatted",status="old",action="read")
  do i=1,13
    read(1,*)
  end do

  !now loop over each ensemble member and write out a text file for each
  do e=1,nmembers

    ii = 1
    do i=fhr_start,fhr_end_orig
      if (mod(i,deltat) .eq. 0 .AND. i .le. fhr_end) then
        !read(1,fmt="(3x,f4.0,f6.1,2(f7.2),f7.1)") col1,col2,col3,col4,col5
        read(1,fmt="(8x,f4.0,f6.1,2(f7.2),f7.1)") col1,col2,col3,col4,col5
        mc_hour(ii) = col1
        vmax(ii) = col2
        lat(ii) = col3
        lon(ii) = 360.0-col4  !deg W is positive
        rmw(ii) = 1.852*col5  !convert nmi to km
        ii = ii+1
      else
        read(1,*)
      end if
    end do

    write(out_file,'(a,i4.4,a)') "storm_data_",e,".txt"
    open(2,file=trim(output_dir)//out_file,form="formatted",status="replace",action="write")
    !PHRAM does not currently use the year, month, day, hour, or pressure, so just use 0 as placeholders here
    do i=1,ntimes
      write(2,fmt="(i3,4(i3.2),2(f8.2),5(f6.1))") int(times(i)),0,0,0,0,lat(i),lon(i),&
                                                  0.0,vmax(i),shr_mag(i),shr_dir(i),rmw(i)
    end do
    close(2)

  end do
  close(1)

end program create_storm_data
