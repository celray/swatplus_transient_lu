      subroutine hyd_reread_connect(con_file, obtyp, nspu1, nspu)
    
      !  con_file ==> connect file for spatial object
      !  nspu     ==> number of spatial units
      !  nspu1    ==> first object number of the spatial unit
      !  nhyds    ==> number of hydrographs for each object
      !  ndsave   ==> number of days of hydrographs to save for subdaily

      use hydrograph_module
      use constituent_mass_module
      use time_module
      use climate_module
      use maximum_data_module
      
      implicit none
      
      !integer, intent(in) :: nhyds    !           |
      !integer, intent(in) :: ndsave   !           |
      integer, intent(in) :: nspu     !           | 
      integer, intent(in) :: nspu1    !           |
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character (len=16) :: namedum   !           |
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      character (len=16) ::con_file   !           |
      character (len=3) :: ihtyp      !           |
      character (len=8) :: obtyp      !           |
      integer :: isp                  !none       |counter
      integer :: cmd_prev             !none       |previous command (object) number
      integer :: ob1                  !none       |beginning of loop
      integer :: ob2                  !none       |ending of loop
      integer :: iob
      integer :: i                    !none       |counter
      integer :: nout                 !           |       
      integer :: iout                 !           |       
      integer :: k                    !           |
      integer :: ics                  !           |
      integer :: ihyd                 !           |hydrograph counter
      integer :: npests               !           |pesticides counter
      integer :: npaths               !           |pathogens counter
      integer :: nmetals              !           |heavy metals counter
      integer :: nsalts			      !           |salts counter
      
      eof = 0
      imax = 0
      cmd_prev = 0
  
      !! read hru spatial data
      inquire (file=con_file, exist=i_exist)
      if (i_exist ) then
        do
          open (4565,file=con_file)
          read (4565,*,iostat=eof) titldum
          if (eof < 0) exit
          read (4565,*,iostat=eof) header
          if (eof < 0) exit

          if (nspu > 0) then
            ob1 = nspu1
            ob2 = nspu1 + nspu - 1

            do i = ob1, ob2
              ob(i)%typ = obtyp

              read (4565,*,iostat=eof) ob(i)%num, ob(i)%name, ob(i)%gis_id, ob(i)%area_ha, ob(i)%lat, ob(i)%long, ob(i)%elev,   &
                ob(i)%props, ob(i)%wst_c, ob(i)%constit, ob(i)%props2, ob(i)%ruleset, ob(i)%src_tot
              if (eof < 0) exit

              read (4565,*,iostat=eof) ob(i)%num, ob(i)%name, ob(i)%gis_id, ob(i)%area_ha, ob(i)%lat, ob(i)%long, ob(i)%elev,    &
                ob(i)%props, ob(i)%wst_c, ob(i)%constit, ob(i)%props2, ob(i)%ruleset, ob(i)%src_tot,      &
                (ob(i)%obtyp_out(isp), ob(i)%obtypno_out(isp), ob(i)%htyp_out(isp),                       &
                ob(i)%frac_out(isp), isp = 1, nout)
              if (eof < 0) exit
        
            end do
          endif
          exit
        enddo
      endif
      
      
      close (4565)
      
      return
      end subroutine hyd_reread_connect