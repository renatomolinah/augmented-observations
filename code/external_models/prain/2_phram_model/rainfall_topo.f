      PROGRAM rainfall_topo
!=======================================================================
!code name: rainfall_topo.f

!manuel (Manuel.Lonfat@rms.com)
!last modified: 01.14.2005, 
! modified for Puerto Rico: 7.23.2008; revised 8.12.2008 FDM (frank.marks@noaa.gov)
! Converted to FORTRAN 77 by Bob Kuligowski 5/21/2014

! Bug fixes by Bob Kuligowski:
! 1. Account for latitudinal differences in width of 1 degree longitude
! 2. Adjust rainfall field for shear during derivation in compass
!     coordinates instead of attempting to adjust for the shear during
!     the reprojection to Cartesian coordinates--which makes no physical
!     sense because it takes the wind vector components from the wrong
!     part of the storm circulation
! 3. Subtract rain instead of adding it (by subtracting a negative
!    elevation change) for downslope
! 4. Reproject the rainfall rates onto the grid by converting from
!     COMPASS coordinates (clockwise starting due N) rather than
!     CYLINDRICAL coordinates (counterclockwise starting due E) since
!     the wind field and shear are in compass coordinates.
!
! Edits & bug fixes by BDM (bmcnoldy@miami.edu) 5/2018
! 1. Suggested alternate URL for topo data (original was unreachable)
! 2. In r_0 equation for tropical storms, 
!     changed "(495./2. + 25.)*2." to "(495./2. + 25.)**2." 
! 3. In second ELSE statement in parameterized rainfall,
!     changed "WindSpeedMax_t0 .LT. 95.0" to "WindSpeedMax_t0 .LE. 95.0"
! 4. In main loop over lead times,
!     changed hard-coded "Lead = 3" to "Lead = INIT_TIME_STEP"
! 5. In the parameterized rainfall (r_0), 
!     defined U and Tm to have the max rainrate depend linearly on 
!     intensity (rather than three bulk categories/coefficients)
! 6. Set minimum VMAX to 35 kt so normalized max wind is always >=1
! 7. Set maximum RMW to 100 km so r<RMW rainrates don't decay too much
! 8. Due to 7, adjusted r_0 coefficients by factor of 1.5/1.25/1.0
! 9. Use TOPO_ERR to handle track outside of topo domain 
!     =0: ignore and keep going (not bad if topo domain is large)
!     =1: STOP and throw exit code 1

! Recommended compiling: gfortran -o phram.exe rainfall_topo.f

!-----------------------------------------------------------------------
!Inputs (geo_top1.txt, storm_data.txt):
! geo_top1.txt: ASCII Topography grid, read N-S.  Specs below under
!                'Topography grid specifications'

! storm_data.txt: ASCII storm track information in 6-h increments:
!                 Field 1: Forecast lead time (h)
!                 Field 2: 2-digit year
!                 Field 3: 2-digit month
!                 Field 4: 2-digit day of month
!                 Field 5: 2-digit hour (GMT)
!                 Field 6: Latitude (degrees with N positive)
!                 Field 7: Longitude (degrees with W *positive*)
!                 Field 8: Central pressure (mb)
!                 Field 9: Max wind speed (kts)
!                 Field 10: 850-200 hPa shear amplitude (kts)
!                 Field 11: 850-200 hPa shear direction (degrees)
!                 Field 12: Radius (km) of maximum wind speed

!-----------------------------------------------------------------------
!Outputs (output.txt):
! output.bin: GrADS binary accumulated rainfall (mm) on the rainfall
!              rainfall grid, written S-N.  Specs below under 'Rainfall
!              grid specifications'

! output.txt: ASCII accumulated rainfall (mm) on the rainfall grid,
!              written S-N.  Specs below under 'Rainfall grid
!              specifications'

!=======================================================================
! ArcGIS topography Master interpolated topo created using Design-a-Grid
! http://www.ngdc.noaa.gov/mgg/gdas/gd_designagrid.html
! OR
! https://maps.ngdc.noaa.gov/viewers/wcs-client/
! ASCII Raster Format: row by row series of space-delimited ASCII
! elevation/depth in meters, top to bottom until the last row (lat,lon).
! Georges used ETOPO01 (1/60 deg) from 80-50W,  0-30N (1801x1801)
! Western Atlantic, same dataset, but 110-50W, 10-50N (3601x2401)
!=======================================================================
      IMPLICIT NONE

!== PARAMETERS =========================================================
!-- Topography grid specifications (1/60 deg)---------------------------
      INTEGER NX_TOPO_GRID, NY_TOPO_GRID ! Topography grid dimensions
      PARAMETER (NX_TOPO_GRID = 4801, NY_TOPO_GRID = 2401)
      REAL*4 TOPO_GRID_MIN_LAT     ! Minimum latitude in topography grid
      PARAMETER (TOPO_GRID_MIN_LAT = 10.0)
      REAL*4 TOPO_GRID_MAX_LAT     ! Maximum latitude in topography grid
      PARAMETER (TOPO_GRID_MAX_LAT = 50.0)
      REAL*4 TOPO_GRID_MIN_LON     ! Minimum longitude (W) in topo grid
      PARAMETER (TOPO_GRID_MIN_LON = -130.0)
      REAL*4 TOPO_GRID_MAX_LON     ! Maximum longidude (W) in topo grid
      PARAMETER (TOPO_GRID_MAX_LON = -50.0)

!-- Rainfall grid specifications (1/60 deg)----------------------------
      INTEGER NX_RAIN_GRID, NY_RAIN_GRID ! Rainfall grid dimensions
      PARAMETER (NX_RAIN_GRID = 2401, NY_RAIN_GRID = 1801)
      REAL*4 RAIN_GRID_MIN_LAT     ! Minimum latitude in rainfall grid
      PARAMETER (RAIN_GRID_MIN_LAT = 20.0)
      REAL*4 RAIN_GRID_MAX_LAT     ! Maximum latitude in rainfall grid
      PARAMETER (RAIN_GRID_MAX_LAT = 50.0)
      REAL*4 RAIN_GRID_MIN_LON     ! Min lon (W negative) in rainfall grid
      PARAMETER (RAIN_GRID_MIN_LON = -105.0)
      REAL*4 RAIN_GRID_MAX_LON     ! Max lon (W negative) in rainfall grid
      PARAMETER (RAIN_GRID_MAX_LON = -65.0)

!!     use these values for Maria (Puerto Rico)
!      PARAMETER (NX_RAIN_GRID = 1201, NY_RAIN_GRID = 1201)
!      PARAMETER (RAIN_GRID_MIN_LAT = 10.0)
!      PARAMETER (RAIN_GRID_MAX_LAT = 30.0)
!      PARAMETER (RAIN_GRID_MIN_LON = -75.0)
!      PARAMETER (RAIN_GRID_MAX_LON = -55.0)

!-- Input storm track specifications (6 hourly)-----------------------
      INTEGER NTRACKFIELDS         ! No. of fields in storm track data
      PARAMETER (NTRACKFIELDS = 12)
      INTEGER NTRACKTIMES          ! # of time periods in storm track data
!--21 for 120h, 19 for 108h, 17 for 96h, 15 for 84h, 13 for 72h, 11 for 60h, 9 for 48h
      PARAMETER (NTRACKTIMES = 21)
      INTEGER INIT_TIME_STEP       ! First time read in track forecast
      PARAMETER (INIT_TIME_STEP = 1)
      REAL*4 TIME_STEP             ! Time step (h)
      PARAMETER (TIME_STEP = 0.25)

!SHOULD NOT NEED TO CHANGE ANYTHING BELOW (NAME OF INPUT TEXT FILES?)
!-- Cylindrical processing grid specifications and scaling -------------
!    This is a finer grid than in the MATLAB original because the 
!    conversion from cylindrical to lat / lon is not done via
!    interpolation as in MATLAB, but by sampling the cylindrical points 
!    from the lat / lon rainfall grid. Without a much finer grid,
!    there will be a lot of holes in the lat / lon rainfall grid.
      INTEGER N_RADII              ! No. of radii in cylindrical grid
      PARAMETER (N_RADII = 500)
      REAL*4 SCALE_RADIUS          ! Convert radius index to real
      PARAMETER (SCALE_RADIUS = 1.0)
      INTEGER N_AZIMUTH            ! No. of azimuths in cylindrical grid
      PARAMETER (N_AZIMUTH = 1440)
      REAL*4 SCALE_AZIMUTH         ! Convert azimuth index to real
      PARAMETER (SCALE_AZIMUTH = 0.25)

!-- Other conversion parameters ----------------------------------------
      REAL*4 KM_PER_DEG_LAT        ! Number of km in a degree latitude
      PARAMETER (KM_PER_DEG_LAT = 111.12)
      REAL*4 PI                    ! Pi
      PARAMETER (PI = 3.1415926536)
      REAL*4 MPS_TO_KTS            ! Convert meters / sec to knots
      PARAMETER (MPS_TO_KTS = 1.94384449)
      REAL*4 MPS_TO_KPH            ! Convert meters / sec to km / hour
      PARAMETER (MPS_TO_KPH = 3.6)

!== VARIABLES ==========================================================
!-- Input from topography file -----------------------------------------
      REAL*4 Elevation(NX_TOPO_GRID,NY_TOPO_GRID) ! Topography
      INTEGER TOPO_ERR             ! if storm outside of topo domain,
                                   ! set below, 0: keep going and ignore
                                   ! set below, 1: STOP with exit code 1

!-- Input from storm track file ----------------------------------------
      REAL*4 track_data(NTRACKTIMES, NTRACKFIELDS) ! Storm track data

!-- Derived values from storm track file -------------------------------
      REAL*4 CenterLat_t0          ! Current center latitude (N positive)
      REAL*4 CenterLon_t0          ! Current center longitude (W positive)
      REAL*4 CenterLat_tminus15    ! Center latitude at prev time step
      REAL*4 CenterLon_tminus15    ! Center longitude at prev time step
      REAL*4 WindSpeedMax_t0       ! Current max wind speed (kts)
      REAL*4 ShearAmplitude        ! Shear amplitude
      REAL*4 ShearDirection        ! Shear direction
      REAL*4 RadiusOfMaxWinds      ! Radius of maximum winds
      REAL*4 StormDeltaX           ! Storm x motion (km) in 1 time step
      REAL*4 StormDeltaY           ! Storm y motion (km) in 1 time step

!-- Output -------------------------------------------------------------
      REAL*4 RainGridTotal(NX_RAIN_GRID,NY_RAIN_GRID) ! Total rain

!-- Processing variables -----------------------------------------------
      INTEGER iTrackTimes          ! Track time counter
      INTEGER iTrackFields         ! Track fields counter
      INTEGER ixRainGrid           ! Rain grid column
      INTEGER iyRainGrid           ! Rain grid row
      INTEGER intervLength         ! Interval in track fcst times (h)
      INTEGER nTimeSteps           ! # time steps between 2 fcst times
      INTEGER Lead                 ! Lead time counter
      REAL*4 interpTime            ! Interpolated (b/w leads) counter
      INTEGER iTopoX, iTopoY       ! Coordinates on topography grid
      INTEGER nPoints(NX_RAIN_GRID,NY_RAIN_GRID) ! # pts in RainGridRain

!-- Radius and azimuth counters and actual values ---------------------
      INTEGER IdxRadius            ! Radius index / counter
      REAL*4 RealRadius            ! Radius (km)
      INTEGER IdxAzimuth           ! Azimuth index / counter
      REAL*4 RealAzimuth           ! Azimuth (degrees)

!-- R-CLIPER coefficients ----------------------------------------------
      REAL*4 U                     ! normalized maximum wind (kt)
      REAL*4 Tm                    ! maximum rainrate at R=Rm (mm/h)
      REAL*4 c1, c2, c3, c4        ! fitting coefficients for r_0
      REAL*4 COEF_ADJ              ! subjective adjustment to r_0 coefs
      REAL*4 r_0(N_RADII)          ! Unsheared R-CLIPER rain rate (mm/h)
      REAL*4 a_1(N_RADII)          ! Fourier coefficient a for wave# 1
      REAL*4 b_1(N_RADII)          ! Fourier coefficient b for wave# 1
      REAL*4 a_2(N_RADII)          ! Fourier coefficient a for wave# 2
      REAL*4 b_2(N_RADII)          ! Fourier coefficient b for wave# 2

!-- R-CLIPER rain rates (mm/h) in cylindrical coordinates --------------
      REAL*4 rain(N_RADII,N_AZIMUTH)

!-- Pixel lat / lon relative to storm center on cylindrical grid -------
      REAL*4 rainLat_relative(N_RADII,N_AZIMUTH) ! Lat relative to center
      REAL*4 rainLon_relative(N_RADII,N_AZIMUTH) ! Lon relative to center

!-- Absolute pixel lat / lon on cylindrical grid -----------------------
      REAL*4 rainLat_t0(N_RADII,N_AZIMUTH) ! Latitude at current time step
      REAL*4 rainLon_t0(N_RADII,N_AZIMUTH) ! Latitude at current time step
      REAL*4 rainLat_tminus15(N_RADII,N_AZIMUTH) ! Lat at prev time step
      REAL*4 rainLon_tminus15(N_RADII,N_AZIMUTH) ! Lon at prev time step

!-- Time change in pixel location during previous time step ------------
      REAL*4 deltaX(N_RADII,N_AZIMUTH) ! Chg in parcel x (km) / time step
      REAL*4 deltaY(N_RADII,N_AZIMUTH) ! Chg in parcel y (km) / time step

!-- Topography in cylindrical coordinates centered on current storm pos
      REAL*4 StormRelativeElev_t0(N_RADII,N_AZIMUTH)

!-- Topography in cylindrical coords centered on storm 1 time step ago -
      REAL*4 StormRelativeElev_tminus15(N_RADII,N_AZIMUTH)

!-- Time change in elevation for storm pixel in last time step ---------
      REAL*4 ElevationChange(N_RADII,N_AZIMUTH)

!-- R-CLIPER rain rate adjusted for topography on lat / lon grid -------
      REAL*4 RainGridRain(NX_RAIN_GRID,NY_RAIN_GRID)


!== MAIN PROGRAM =======================================================
!-- Read topography from file ------------------------------------------
      TOPO_ERR = 0       !0/1 to ignore/stop if storm out of topo domain
      OPEN (10, FILE='US_topo.txt', STATUS='old', ERR=97)
      READ (10,*) Elevation
      CLOSE (10)

!-- Initialize accumulation --------------------------------------------
      DO iyRainGrid = 1, NY_RAIN_GRID
         DO ixRainGrid = 1, NX_RAIN_GRID
            RainGridTotal(ixRainGrid,iyRainGrid) = 0.
         END DO
      END DO
 
!-- Read storm track data ---------------------------------------------
      OPEN (20, FILE='storm_data.txt', STATUS='old', ERR=98)
      DO iTrackTimes = 1, NTRACKTIMES
         READ (20,*) (track_data(iTrackTimes,iTrackFields),
     +    iTrackFields=1,NTRACKFIELDS)
      END DO
      CLOSE (20)

!--- Process storm track data ------------------------------------------
!--- Initial values ----------------------------------------------------
      intervLength = 6
      nTimeSteps = NINT(1. / TIME_STEP) * intervLength
      CenterLat_tminus15 = track_data(INIT_TIME_STEP,6)
      CenterLon_tminus15 = -track_data(INIT_TIME_STEP,7)

!--- Set minimum VMAX to 35kt and maximum RMW to 100km -----------------
      DO iTrackTimes = 1, NTRACKTIMES
         IF (track_data(iTrackTimes,9) .LT. 35.0) THEN
           track_data(iTrackTimes,9) = 35.0
         END IF
         IF (track_data(iTrackTimes,12) .GT. 100.0) THEN
           track_data(iTrackTimes,12) = 100.0
         END IF
      END DO      

!--- Loop through lead times in storm track file -----------------------
!      PRINT *,'    LAT   LON  VMAX   SPD'
!      PRINT *,'========================='
      DO Lead = INIT_TIME_STEP, NTRACKTIMES - 1
         interpTime = 0.
         intervLength = track_data(Lead+1,1) - track_data(Lead,1)
         nTimeSteps = NINT(1. / TIME_STEP) * intervLength

!--- Values at exact lead time (no interpolation necessary) -----------
         CenterLat_t0     =  track_data(Lead,6)
         CenterLon_t0     = -track_data(Lead,7)
         ShearAmplitude   =  track_data(Lead,10)
         ShearDirection   =  track_data(Lead,11)
         RadiusOfMaxWinds =  track_data(Lead,12)

!-- Compute zonal storm motion from change in longitude in storm track -
         StormDeltaX = (track_data(Lead+1,7) - track_data(Lead,7)) *
     +    KM_PER_DEG_LAT * COS(CenterLat_t0 * PI / 180.) /
     +    (FLOAT(intervLength) / TIME_STEP)

!-- Compute meridional storm motion from change in latitude in track ---
         StormDeltaY = (track_data(Lead+1,6) - track_data(Lead,6)) *
     +    KM_PER_DEG_LAT / (FLOAT(intervLength) / TIME_STEP)

!-- print storm data at current lead time
!         PRINT 1, Lead, CenterLat_t0, CenterLon_t0, track_data(Lead,9),
!     +     SQRT(StormDeltaX*StormDeltaX+StormDeltaY*StormDeltaY)
!   1     FORMAT (I2,F6.1,F6.1,F6.0,F6.1)

!-- Loop through time steps in between storm track intervals -----------
   9     IF (interpTime .LT. intervLength) THEN
  
!-- Linear interpolation of track database to time step resolution -----
!-- Interpolate latitude (deg N is positive ) --------------------------
            CenterLat_t0 = track_data(Lead,6)  +
     +       (track_data(Lead+1,6) - track_data(Lead,6))  *
     +       ((1. / TIME_STEP) * interpTime) / nTimeSteps

!-- Interpolate longitude (W is negative, but positive in input file!) -
            CenterLon_t0 = -track_data(Lead,7)  +
     +       (-track_data(Lead+1,7) + track_data(Lead,7))  *
     +       ((1. / TIME_STEP) * interpTime) / nTimeSteps

!-- Interpolate shear amplitude (kts) ----------------------------------
            ShearAmplitude = track_data(Lead,10) +
     +       (track_data(Lead+1,10) - track_data(Lead,10)) *
     +       ((1. / TIME_STEP) * interpTime) / nTimeSteps

!-- Interpolate maximum wind speed (kts) -------------------------------
            WindSpeedMax_t0 = track_data(Lead,9)  +
     +       (track_data(Lead+1,9)  - track_data(Lead,9))  *
     +       ((1. / TIME_STEP) * interpTime) / nTimeSteps 

!-- Interpolate radius of maximum wind speed ---------------------------
            RadiusOfMaxWinds = track_data(Lead,12) +
     +       (track_data(Lead+1,12) - track_data(Lead,12)) *
     +       ((1. / TIME_STEP) * interpTime) / nTimeSteps

!-- Increment time step ------------------------------------------------
            interpTime = interpTime + TIME_STEP
    
!-- Interpolate shear direction, reversing it by 180 degrees and making
!-- sure to keep values within range -----------------------------------
            IF ((track_data(Lead+1,11) - track_data(Lead,11)) .GT. 180)
     +       THEN
               ShearDirection = track_data(Lead,11) +
     +          ((track_data(Lead+1,11) - track_data(Lead,11)) - 360.) *
     +          ((1. / TIME_STEP) * interpTime) / nTimeSteps;
               IF (ShearDirection.LT.0.) ShearDirection = 360. -
     +          ShearDirection
            ELSE IF ((track_data(Lead+1,11) - track_data(Lead,11)) .LT.
     +       -180) THEN
               ShearDirection = track_data(Lead,11) + (360 -
     +          (track_data(Lead+1,11) - track_data(Lead,11))) *
     +          ((1. / TIME_STEP) * interpTime) / nTimeSteps
               IF (ShearDirection .GT. 360.) ShearDirection =
     +          ShearDirection - 360.
            ELSE
               ShearDirection = track_data(Lead,11) +
     +          (track_data(Lead+1,11) - track_data(Lead,11)) *
     +          ((1. / TIME_STEP) * interpTime) / nTimeSteps
            ENDIF
            ShearDirection = ShearDirection + 180.
            IF (ShearDirection .GT. 360.) ShearDirection =
     +       ShearDirection - 360.

!-- Derive change in parcel position during previous time step based on
!--- wind field using Willoughby (2002) simplified ---------------------
            DO IdxRadius = 1, N_RADII
               DO IdxAzimuth = 1, N_AZIMUTH
                  RealRadius = float(IdxRadius) * SCALE_RADIUS + 5.
                  RealAzimuth = FLOAT(IdxAzimuth) * SCALE_AZIMUTH
                  IF (RealRadius .LE. RadiusOfMaxWinds) THEN
                     deltaX(IdxRadius,IdxAzimuth) =
     +                -(MPS_TO_KPH * TIME_STEP) *
     +                WindSpeedMax_t0 / MPS_TO_KTS *
     +                (RealRadius / RadiusOfMaxWinds) *
     +                COS(RealAzimuth * PI / 180.) + StormDeltaX
                     deltaY(IdxRadius,IdxAzimuth) =
     +                (MPS_TO_KPH * TIME_STEP) *
     +                WindSpeedMax_t0 / MPS_TO_KTS *
     +                (RealRadius / RadiusOfMaxWinds) *
     +                SIN(RealAzimuth * PI / 180.) + StormDeltaY
                  ELSE
                     deltaX(IdxRadius,IdxAzimuth) =
     +                -(MPS_TO_KPH * TIME_STEP) *
     +                WindSpeedMax_t0 / MPS_TO_KTS *
     +                EXP(-(RealRadius - RadiusOfMaxWinds) / 250.) *
     +                COS(RealAzimuth * PI / 180.) + StormDeltaX
                     deltaY(IdxRadius,IdxAzimuth) =
     +                (MPS_TO_KPH * TIME_STEP) *
     +                WindSpeedMax_t0 / MPS_TO_KTS *
     +                EXP(-(RealRadius - RadiusOfMaxWinds) / 250.) *
     +                SIN(RealAzimuth * PI / 180.) + StormDeltaY
                  ENDIF
               END DO
            END DO
    
!-- Define parameterization functions for R-CLIPER rain rates ----------
            DO IdxRadius = 1, N_RADII
               RealRadius = FLOAT(IdxRadius) * SCALE_RADIUS + 5.

!-- No rain within 5 km of storm center --------------------------------
               IF (RealRadius.lt.5.) THEN
                  r_0(IdxRadius) = 0.
                  a_1(IdxRadius) = 0.
                  a_2(IdxRadius) = 0.
                  b_1(IdxRadius) = 0.
                  b_2(IdxRadius) = 0.

!-- Parameterized rain more than 5 km from storm center ----------------
!-- values are in mm/hr ------------------------------------------------
               ELSE
                  U = 1.0+(WindSpeedMax_t0-35.0)/33.0
                  Tm = -2.73 + 4.49*U
                  IF (WindSpeedMax_t0 .LT. 65.0) THEN
                     COEF_ADJ = 1.5 
                     c1 = 125.0 * COEF_ADJ
                     c2 = 0.80  / COEF_ADJ
                     c3 = 205.0 * COEF_ADJ
                     c4 = 2.00  * COEF_ADJ 
                     r_0(IdxRadius) = Tm *
     +                EXP(-(RealRadius - (RadiusOfMaxWinds)) ** 2. /
     +                (c1) ** 2.) + c2 *
     +                EXP(-(RealRadius - (RadiusOfMaxWinds + c3)) **
     +                2. / (RealRadius / c4 + 25.) ** 2.) - (Tm *
     +                EXP(-(495. - (RadiusOfMaxWinds)) ** 2. /
     +                (c1) ** 2.) + c2 *
     +                EXP(-(495. - (RadiusOfMaxWinds + c3)) ** 2. /
     +                (495. / c4 + 25.) ** 2.))
                     IF (ShearAmplitude .LT. 9.7) THEN
                        a_1(IdxRadius) = -17.5 *
     +                   EXP(-(RealRadius - 100.) ** 2. /
     +                   ((RealRadius + 50.) * 0.5) ** 2.) + 7.5
                        b_1(IdxRadius) = 1.1 * (RealRadius - 5.) **
     +                   0.6 - EXP((RealRadius - 125.) / 100.)
                        a_2(IdxRadius) = 4. -10. *
     +                   EXP(-(RealRadius - 245.) ** 2. / 250. ** 2.)
                        b_2(IdxRadius) = 1. -10. *
     +                   EXP(-(RealRadius - 335.) ** 2. / 60. ** 2.)
                     ELSE IF ((ShearAmplitude .GE. 9.7) .AND.
     +                (ShearAmplitude .LT. 19.4)) THEN
                        a_1(IdxRadius) = -20. * EXP(-(RealRadius - 65.)
     +                  ** 2. /
     +                   ((RealRadius + 50.) * 0.65) ** 2.) + 2.5
                        b_1(IdxRadius) = 1.1 *
     +                   (RealRadius - 5.) ** 0.75 -
     +                   ((RealRadius - 25.) / 47.5) ** 2.
                        a_2(IdxRadius) = -2.5 -
     +                   12.5 * EXP(0. - (RealRadius - 245.) ** 2. /
     +                   100. ** 2.)
                        b_2(IdxRadius) = -1.5 * (RealRadius - 5. ) **
     +                   0.25
                     ELSE
                        a_1(IdxRadius) = -35. * EXP(-(RealRadius - 85.)
     +                   ** 2. /
     +                   ((RealRadius + 150.) * 0.35) ** 2.) -5.
                        b_1(IdxRadius) = 1.1 * (RealRadius - 5.) **
     +                   0.75 - ((RealRadius - 25.) / 50.) ** 2. + 12.5
                        a_2(IdxRadius) = -1.5 - 20. *
     +                   EXP(-(RealRadius - 205) ** 2. / 100. ** 2.)
                        b_2(IdxRadius) = -2.5 * (RealRadius - 5.) **
     +                   0.32
                     ENDIF
                  ELSE IF ((WindSpeedMax_t0 .GE. 65.0) .AND.
     +             (WindSpeedMax_t0 .LE. 95.0)) THEN
                     COEF_ADJ = 1.25 
                     c1 = 100.0 * COEF_ADJ
                     c2 = 1.50  / COEF_ADJ
                     c3 = 135.0 * COEF_ADJ
                     c4 = 2.25  * COEF_ADJ 
                     r_0(IdxRadius) = Tm *
     +                EXP(-(RealRadius - RadiusOfMaxWinds) ** 2. /
     +                (c1) ** 2.) +
     +                c2 * EXP(-(RealRadius -
     +                (RadiusOfMaxWinds + c3 )) ** 2. /
     +                (RealRadius / c4 + 25.) ** 2.) -
     +                (Tm * EXP(-(495. -
     +                RadiusOfMaxWinds) ** 2. / (c1) ** 2) +
     +                c2 * EXP(-(495. - (RadiusOfMaxWinds + c3))
     +                ** 2. /
     +                (495. / c4 + 25.) ** 2.))
!-- this case is plotted as solid curves in Figure 1 of Lonfat et al (2007)
                     IF (ShearAmplitude .LT. 9.7) THEN
                        a_1(IdxRadius) = -15. *
     +                   EXP(-(RealRadius - 100.) ** 2. /
     +                   ((RealRadius + 300.) * 0.25) ** 2.) +
     +                   (-8. + RealRadius / 20.) -
     +                   EXP((RealRadius - 175.) / 85.)
                        b_1(IdxRadius) = 1.1 * (RealRadius - 5.) ** 0.35
                        a_2(IdxRadius) = 10. - 8. *
     +                  EXP((RealRadius) ** 2. / 600. ** 2.)
                        b_2(IdxRadius) = 6.5 - 8. *
     +                   EXP(-(RealRadius - 245.) ** 2. / 250. ** 2.) -
     +                   EXP((RealRadius - 250.) / 100.)
!-- this case is plotted as dashed curves in Figure 1 of Lonfat et al (2007)
                     ELSE IF ((ShearAmplitude .GE. 9.7) .AND.
     +                (ShearAmplitude .LT. 19.4)) THEN
                        a_1(IdxRadius) = -15. *
     +                   EXP(-(RealRadius - 100.) ** 2. /
     +                   ((RealRadius + 300.) * 0.2) ** 2.) - 9. -
     +                   EXP((RealRadius - 250.) / 100.)
                        b_1(IdxRadius) = 1.1 *
     +                   (RealRadius - 5.) ** 0.75 - 
     +                   2.5 * (RealRadius / 50.) ** 1.5 + 2.5
                        a_2(IdxRadius) = -2.5 - 10. *
     +                   EXP(-(RealRadius - 225.) ** 2. /
     +                   (100. - (RealRadius - 100.) / 10.) ** 2.)
                        b_2(IdxRadius) = -1.25 *
     +                   (RealRadius - 5.) ** 0.2 -
     +                   EXP((RealRadius - 400.) / 100.)
!-- this case is plotted as dotted curves in Figure 1 of Lonfat et al (2007)
                     ELSE
                        a_1(IdxRadius) = -25. *
     +                   EXP(-(RealRadius - 100.) ** 2. /
     +                   ((RealRadius + 100.) * 0.75) ** 2.) - 9. +
     +                   EXP((RealRadius - 200.) / 75.)
                        b_1(IdxRadius) = 1.1 *
     +                   (RealRadius - 5.) ** 0.7 -
     +                   ((RealRadius - 25.) / 75.) ** 2. + 5.
                        a_2(IdxRadius) = - 1.5 - 16. *
     +                   EXP(-(RealRadius - 375.) ** 2 / 200. ** 2)
                        b_2(IdxRadius) = - 2.5 * (RealRadius -5) ** 0.23
                     ENDIF
                  ELSE
                     COEF_ADJ = 1.0 
                     c1 = 75.0  * COEF_ADJ
                     c2 = 3.25  / COEF_ADJ
                     c3 = 115.0 * COEF_ADJ
                     c4 = 3.00  * COEF_ADJ 
                     r_0(IdxRadius) = Tm *
     +               EXP(-((RealRadius - RadiusOfMaxWinds) ** 2. /
     +                (c1) ** 2.)) + c2 *
     +               EXP(-((RealRadius - (RadiusOfMaxWinds + c3))
     +                ** 2. / (RealRadius / c4 + 25.) ** 2.)) -
     +               (Tm * EXP(-((495. - RadiusOfMaxWinds) ** 2. /
     +               (c1) ** 2.)) + c2 *
     +               EXP(-((495. - (RadiusOfMaxWinds + c3)) ** 2. /
     +               (495. / c4 + 25.) ** 2.)))
                     IF (ShearAmplitude .LT. 9.7) THEN
                        a_1(IdxRadius) = -15. *
     +                   EXP(-(RealRadius -100.) ** 2. /
     +                   ((RealRadius + 300.) * 0.5) ** 2.) +
     +                   (5. + RealRadius / 20.) -2. *
     +                   EXP((RealRadius - 75.) / 150.)
                        b_1(IdxRadius) = 1.1 * (RealRadius - 5.) ** 0.35
                        a_2(IdxRadius) = 1.5 - 11.5 * 
     +                   EXP(-(RealRadius - 375.) ** 2. / 125 ** 2.)
                        b_2(IdxRadius) = 6.5 - 8. * 
     +                   EXP(-(RealRadius - 245.) ** 2. / 250 ** 2.) +
     +                   22.5 * EXP(-(RealRadius - 410.) ** 2. /
     +                   50. ** 2.)
                     ELSE IF ((ShearAmplitude .GE. 9.7) .AND.
     +                (ShearAmplitude .LT. 19.4)) THEN
                        a_1(IdxRadius) = -15. *
     +                   EXP(-(RealRadius - 150.) ** 2. /
     +                   ((RealRadius + 500.) * 0.15) ** 2.) - 11.
                        b_1(IdxRadius) = 1.1 *
     +                   ((RealRadius - 5.) / 10.) ** 1.3 -
     +                   7. * ((RealRadius - 50.) / 100.) ** 2.
                        a_2(IdxRadius) = 1. - 10. *
     +                   EXP(-(RealRadius - 350.) ** 2. / 125. ** 2.)
                        b_2(IdxRadius) = 4.5 - 20. *
     +                   EXP(-(RealRadius - 300.) ** 2. / 100. ** 2.)
                     ELSE
                        a_1(IdxRadius) = -15. *
     +                   EXP(-(RealRadius -100.) ** 2. /
     +                   ((RealRadius + 300.) * 0.35) ** 2.) - 15.
                        b_1(IdxRadius) = 1.1 *
     +                   (RealRadius - 5.) ** 0.72 -
     +                   ((RealRadius - 25.) / 50.) ** 2. + 5.
                        a_2(IdxRadius) = -1.5 - 20. *
     +                   EXP(-(RealRadius - 245.) ** 2. / 50. ** 2.) +
     +                   15. *
     +                   EXP(-(RealRadius - 340.) ** 2. / 50. ** 2.)
                        b_2(IdxRadius) = -5. -10. *
     +                   EXP(-(RealRadius - 245.) ** 2. / 100. ** 2.)
                     ENDIF
                  ENDIF
               ENDIF
            END DO

!-- Compute R-CLIPER rain rates on cylindrical grid centered on storm --
!-- using compass azimuths ---------------------------------------------
            DO IdxRadius = 1, N_RADII
               DO IdxAzimuth = 1, N_AZIMUTH

!-- Adjust azimuthal coordinate by subtracting the (reversed) shear ----
!-- vector -------------------------------------------------------------
                  RealAzimuth = FLOAT(IdxAzimuth) * SCALE_AZIMUTH -
     +             ShearDirection
                  IF (RealAzimuth.LT.0.) RealAzimuth = RealAzimuth +
     +             360.

!-- Compute R-CLIPER rain rates using Fourier coefficients -------------
!-- why are the a and b coefficients multiplied by r_0/100??  BDM
                  rain(IdxRadius,IdxAzimuth)= r_0(IdxRadius) +
     +             a_1(IdxRadius) * (r_0(IdxRadius) / 100.) *
     +             COS(RealAzimuth * PI / 180. ) +
     +             b_1(IdxRadius) * (r_0(IdxRadius) / 100.) *
     +             SIN(RealAzimuth * PI / 180.) +
     +             a_2(IdxRadius) * (r_0(IdxRadius) / 100.) *
     +             COS(2. * RealAzimuth * PI / 180.) +
     +             b_2(IdxRadius) * (r_0(IdxRadius) /100.) *
     +             SIN(2. * RealAzimuth * PI / 180.)
               END DO
            END DO

!-- Compute lat and lon relative to storm center for each cylindrical --
!-- grid point ---------------------------------------------------------
            DO IdxRadius = 1, N_RADII
               RealRadius = FLOAT(IdxRadius) * SCALE_RADIUS + 5.
               DO IdxAzimuth = 1, N_AZIMUTH
                  RealAzimuth = FLOAT(IdxAzimuth) * SCALE_AZIMUTH
                  rainLat_relative(IdxRadius,IdxAzimuth) = RealRadius *
     +             COS(RealAzimuth * PI / 180.) / KM_PER_DEG_LAT
                  rainLon_relative(IdxRadius,IdxAzimuth) = RealRadius *
     +             SIN(RealAzimuth * PI / 180.) /
     +             (KM_PER_DEG_LAT *
     +             COS(rainLat_relative(IdxRadius,IdxAzimuth) *
     +             PI / 180.))
               END DO
            END DO

!-- Compute absolute coordinates from storm-relative coordinates, and --
!-- storm center position, and position changes ------------------------
            DO IdxRadius = 1, N_RADII
               DO IdxAzimuth = 1, N_AZIMUTH
                  RealAzimuth = FLOAT(IdxAzimuth) * SCALE_AZIMUTH

!-- Absolute lat / lon of each cylindrical grid element at current time
                  rainLat_t0(IdxRadius,IdxAzimuth) = CenterLat_t0 +
     +             rainLat_relative(IdxRadius,IdxAzimuth)

                  rainLon_t0(IdxRadius,IdxAzimuth) = CenterLon_t0 +
     +             rainLon_relative(IdxRadius,IdxAzimuth)

!-- Absolute lat / lon of each cylindrical grid element at previous time
                  rainLat_tminus15(IdxRadius,IdxAzimuth) =
     +             CenterLat_tminus15 +
     +             rainLat_relative(IdxRadius,IdxAzimuth) -
     +             deltaY(IdxRadius,IdxAzimuth) / KM_PER_DEG_LAT

                  rainLon_tminus15(IdxRadius,IdxAzimuth) =
     +             CenterLon_tminus15 +
     +             rainLon_relative(IdxRadius,IdxAzimuth) -
     +             deltaX(IdxRadius,IdxAzimuth) / (KM_PER_DEG_LAT *
     +             COS(CenterLat_tminus15 * PI / 180.))

               END DO
            END DO
      
!-- Apply the topographic adjustment to the rain rates based on the ----
!-- change in elevation of the parcel during the previous time step ----
	    DO IdxRadius = 1, N_RADII
               DO IdxAzimuth = 1, N_AZIMUTH
!-- Location of pixel at current time on topo grid ---------------------
                  iTopoX = NINT((FLOAT(NX_TOPO_GRID - 1) *
     +             (rainLon_t0(IdxRadius,IdxAzimuth) -
     +             TOPO_GRID_MIN_LON)) /
     +             (TOPO_GRID_MAX_LON - TOPO_GRID_MIN_LON)) + 1
                  iTopoY = NY_TOPO_GRID -
     +             NINT((FLOAT(NY_TOPO_GRID - 1) *
     +             (rainLat_t0(IdxRadius,IdxAzimuth) -
     +             TOPO_GRID_MIN_LAT)) /
     +             (TOPO_GRID_MAX_LAT - TOPO_GRID_MIN_LAT))

!-- Determine the elevation of the pixel, setting negative values to 0 -
                  IF ((iTopoX.GE.1).AND.(iTopoX.LE.NX_TOPO_GRID).AND.
     +             (iTopoY.GE.1).AND.(iTopoY.LE.NY_TOPO_GRID)) THEN
                     StormRelativeElev_t0(IdxRadius,IdxAzimuth) =
     +                max(Elevation(iTopoX,iTopoY),0.)
!-- Write out an error message if the pixel is outside the topo map ----
                  ELSE
                     IF (TOPO_ERR.EQ.1) THEN
                        WRITE (*,*) 'CenterLat_t0=',CenterLat_t0
                        WRITE (*,*) 'CenterLon_t0=',CenterLon_t0
                        WRITE (*,*) 'rainLat_t0(IdxRadius,IdxAzimuth)=',
     +                   rainLat_t0(IdxRadius,IdxAzimuth)
                        WRITE (*,*) 'rainLon_t0(IdxRadius,IdxAzimuth)=',
     +                   rainLon_t0(IdxRadius,IdxAzimuth)
                        WRITE (*,*) 'iTopoX =',iTopoX
                        WRITE (*,*) 'iTopoY =',iTopoY
                        WRITE (*,*) 'Current outside topography bounds'
                        STOP 1
                     ENDIF
                  ENDIF

!-- Location of pixel at previous time step on topo grid ---------------
                  iTopoX = NINT((FLOAT(NX_TOPO_GRID - 1) *
     +             (rainLon_tminus15(IdxRadius,IdxAzimuth) -
     +             TOPO_GRID_MIN_LON)) /
     +             (TOPO_GRID_MAX_LON - TOPO_GRID_MIN_LON))
                  iTopoY = NY_TOPO_GRID -
     +             NINT((FLOAT(NY_TOPO_GRID - 1) *
     +             (rainLat_tminus15(IdxRadius,IdxAzimuth) -
     +             TOPO_GRID_MIN_LAT)) /
     +             (TOPO_GRID_MAX_LAT - TOPO_GRID_MIN_LAT)) + 1

!-- Determine the elevation of the pixel, setting negative values to 0 -
                  IF ((iTopoX.GE.1).AND.(iTopoX.LE.NX_TOPO_GRID).AND.
     +             (iTopoY.GE.1).AND.(iTopoY.LE.NY_TOPO_GRID)) THEN
                     StormRelativeElev_tminus15(IdxRadius,IdxAzimuth) =
     +                max(Elevation(iTopoX,iTopoY),0.)
!-- Write out an error message if the pixel is outside the topo map ----
                  ELSE
                     IF (TOPO_ERR.EQ.1) THEN
                        WRITE (*,*)
     +                   'rainLat_tminus15(IdxRadius,IdxAzimuth)=',
     +                   rainLat_tminus15(IdxRadius,IdxAzimuth)
                        WRITE (*,*)
     +                   'rainLon_tminus15(IdxRadius,IdxAzimuth)=',
     +                   rainLon_tminus15(IdxRadius,IdxAzimuth)
                        WRITE (*,*) 'iTopoX =',iTopoX
                        WRITE (*,*) 'iTopoY =',iTopoY
                        WRITE (*,*) 'Previous outside topography bounds'
                        STOP 1
                     ENDIF
                  ENDIF

               END DO
            END DO

!-- Compute change in parcel elevation during the time step ------------
            DO IdxRadius = 1, N_RADII
               DO IdxAzimuth = 1, N_AZIMUTH
                  ElevationChange(IdxRadius,IdxAzimuth) =
     +             StormRelativeElev_t0(IdxRadius,IdxAzimuth) -
     +             StormRelativeElev_tminus15(IdxRadius,IdxAzimuth)
               END DO
            END DO
  
!-- Modify rainfall based on parcel elevation change -------------------
            DO IdxRadius = 1, N_RADII
               DO IdxAzimuth = 1, N_AZIMUTH

!-- Parcel elevation is unchanged or increases (upslope) ---------------
                  IF (ElevationChange(IdxRadius,IdxAzimuth) .GE. 0.0)
     +             THEN
                     rain(IdxRadius,IdxAzimuth) =
     +                rain(IdxRadius,IdxAzimuth) +
     +                rain(IdxRadius,IdxAzimuth) / 10. +
     +                rain(IdxRadius,IdxAzimuth) *
     +                ElevationChange(IdxRadius,IdxAzimuth) / 100.

!-- Parcel elevation decreases (downslope) -----------------------------
                  ELSE
                     rain(IdxRadius,IdxAzimuth) =
     +                rain(IdxRadius,IdxAzimuth) +
     +                rain(IdxRadius,IdxAzimuth) / 10. +
     +                rain(IdxRadius,IdxAzimuth) *
     +                ElevationChange(IdxRadius,IdxAzimuth) / 500.
                  ENDIF
               END DO
            END DO

!-- Interpolate back to rainfall grid ----------------------------------
!-- Initialize rainfall and number of points on rainfall grid ----------
            DO iyRainGrid = 1, NY_RAIN_GRID
               DO ixRainGrid = 1, NX_RAIN_GRID
                  RainGridRain(ixRainGrid,iyRainGrid) = 0.
                  nPoints(ixRainGrid,iyRainGrid) = 0
               END DO
            END DO

!-- Sum up cylindrical pixels in each grid box on rainfall grid --------
            DO IdxRadius = 1, N_RADII
               DO IdxAzimuth = 1, N_AZIMUTH

!-- Determine row in output grid corresponding to cylindrical pixel ----
                  iyRainGrid = 1 +
     +             NINT((rainLat_t0(IdxRadius,IdxAzimuth) -
     +             RAIN_GRID_MIN_LAT) / 
     +             (RAIN_GRID_MAX_LAT - RAIN_GRID_MIN_LAT) *
     +             FLOAT(NY_RAIN_GRID - 1))

!-- Determine column in output grid corresponding to cylindrical pixel -
                  ixRainGrid = 1 +
     +             NINT((rainLon_t0(IdxRadius,IdxAzimuth) -
     +             RAIN_GRID_MIN_LON) / 
     +             (RAIN_GRID_MAX_LON - RAIN_GRID_MIN_LON) *
     +             FLOAT(NX_RAIN_GRID - 1))
                  IF ((ixRainGrid.GE.1).AND.(ixRainGrid.LE.NX_RAIN_GRID)
     +             .AND.(iyRainGrid.GE.1).AND.
     +             (iyRainGrid.LE.NY_RAIN_GRID)) THEN
                     nPoints(ixRainGrid,iyRainGrid) =
     +                nPoints(ixRainGrid,iyRainGrid) + 1
                     RainGridRain(ixRainGrid,iyRainGrid) =
     +                RainGridRain(ixRainGrid,iyRainGrid) +
     +                rain(IdxRadius,IdxAzimuth)
                  ENDIF
               END DO
            END DO

!-- Average all values in cylindrical pixel and add to rain accumulation
!-- RainGrid values are in mm/h, so RainGridTotal divides by TIME_STEP
!--   to get mm.  Final output will be in mm.
            DO iyRainGrid = 1, NY_RAIN_GRID
               DO ixRainGrid = 1, NX_RAIN_GRID
                  IF (nPoints(ixRainGrid,iyRainGrid).GT.0) THEN
                     RainGridRain(ixRainGrid,iyRainGrid) =
     +                RainGridRain(ixRainGrid,iyRainGrid) /
     +                FLOAT(nPoints(ixRainGrid,iyRainGrid))
                     RainGridTotal(ixRainGrid,iyRainGrid) =
     +                RainGridTotal(ixRainGrid,iyRainGrid) +
     +                RainGridRain(ixRainGrid,iyRainGrid) /
     +                (1. / TIME_STEP)
                  ENDIF
               END DO
            END DO

            CenterLat_tminus15 = CenterLat_t0
            CenterLon_tminus15 = CenterLon_t0

!-- Go to next time step -----------------------------------------------
            GOTO 9
         ENDIF
      END DO

!-- Write output in GrADS-ready binary format --------------------------
!      OPEN (30, FILE='R-CLIPER.bin', ACCESS='direct',
!     + FORM='unformatted', STATUS='new',
!     + RECL=4*NX_RAIN_GRID*NY_RAIN_GRID)
!      WRITE (30, REC=1) RainGridTotal
!      CLOSE (30)

!-- Write output in ASCII format ---------------------------------------
      OPEN (30, FILE='R-CLIPER.txt')
      DO iyRainGrid = 1, NY_RAIN_GRID
         WRITE (30,*) (RainGridTotal(ixRainGrid,iyRainGrid),
     +    ixRainGrid = 1, NX_RAIN_GRID)
      END DO
      CLOSE (30)

      GOTO 99
  97  WRITE (*,*) 'Unable to open topography file'
      GOTO 99
  98  WRITE (*,*) 'Unable to open storm track file'

  99  CONTINUE
      END
