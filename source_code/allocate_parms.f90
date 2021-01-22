      subroutine allocate_parms
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine allocates array sizes

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mhyd        |none          |max number of hydrographs
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      use hru_module      
      use time_module
      use hydrograph_module
      use constituent_mass_module
      
!! initialize variables    
      mhyd = 1  !!added for jaehak vars
      mhru = sp_ob%hru
      mch = sp_ob%chan

	  !! Celray James Added provision for deallocating if already allocated - commented 22/01/2020 20:03
	!If (allocated(wnan))  deallocate (wnan)
	!If (allocated(ranrns_hru))  deallocate (ranrns_hru)
	!If (allocated(uno3d))  deallocate (uno3d)
      !If (allocated(uapd))  deallocate (uapd)
      !If (allocated(un2))  deallocate (un2)
      !If (allocated(up2))  deallocate (up2)
      !If (allocated(translt))  deallocate (translt)
      !If (allocated(par))  deallocate (par)
      !If (allocated(htfac))  deallocate (htfac)
      !If (allocated(epmax))  deallocate (epmax)
      !If (allocated(cvm_com))  deallocate (cvm_com)
      !If (allocated(rsdco_plcom))  deallocate (rsdco_plcom)
      !If (allocated(percn))  deallocate (percn)
      !If (allocated(percp))  deallocate (percp)
      !If (allocated(i_sep))  deallocate (i_sep)
      !If (allocated(sep_tsincefail))  deallocate (sep_tsincefail)
      !If (allocated(qstemm))  deallocate (qstemm)
      !If (allocated(bio_bod))  deallocate (bio_bod)
      !If (allocated(biom))  deallocate (biom)
      !If (allocated(rbiom))  deallocate (rbiom)
      !If (allocated(fcoli))  deallocate (fcoli)
      !If (allocated(bz_perc))  deallocate (bz_perc)
      !If (allocated(plqm))  deallocate (plqm)
      !If (allocated(itb))  deallocate (itb)
      !If (allocated(hhqday))  deallocate (hhqday)
      !If (allocated(sol_sumno3))  deallocate (sol_sumno3)
      !If (allocated(sol_sumsolp))  deallocate (sol_sumsolp)
      !If (allocated(iseptic))  deallocate (iseptic)
      !If (allocated(grz_days))  deallocate (grz_days)
      !If (allocated(brt))  deallocate (brt)
      !If (allocated(canstor))  deallocate (canstor)
      !If (allocated(cbodu))  deallocate (cbodu)
      !If (allocated(chl_a))  deallocate (chl_a)
      !If (allocated(cklsp))  deallocate (cklsp)
      !If (allocated(cn2))  deallocate (cn2)
      !If (allocated(cnday))  deallocate (cnday)
      !If (allocated(cumei))  deallocate (cumei)
      !If (allocated(cumeira))  deallocate (cumeira)
      !If (allocated(cumrt))  deallocate (cumrt)
      !If (allocated(cumrai))  deallocate (cumrai)
      !If (allocated(dormhr))  deallocate (dormhr)
      !If (allocated(doxq))  deallocate (doxq)
      !If (allocated(filterw))  deallocate (filterw)
      !If (allocated(hru_ra))  deallocate (hru_ra)
      !If (allocated(hru_rmx))  deallocate (hru_rmx)
      !If (allocated(igrz))  deallocate (igrz)
      !If (allocated(yr_skip))  deallocate (yr_skip)
      !If (allocated(isweep))  deallocate (isweep)
      !If (allocated(phusw))  deallocate (phusw)
      !If (allocated(latno3))  deallocate (latno3)
      !If (allocated(latq))  deallocate (latq)
      !If (allocated(ndeat))  deallocate (ndeat)
      !If (allocated(nplnt))  deallocate (nplnt)
      !If (allocated(orgn_con))  deallocate (orgn_con)
      !If (allocated(orgp_con))  deallocate (orgp_con)
      !If (allocated(ovrlnd))  deallocate (ovrlnd)
      !If (allocated(phubase))  deallocate (phubase)
      !If (allocated(pplnt))  deallocate (pplnt)
      !If (allocated(qdr))  deallocate (qdr)
      !If (allocated(rhd))  deallocate (rhd)
      !If (allocated(sstmaxd))  deallocate (sstmaxd)
      !If (allocated(sedminpa))  deallocate (sedminpa)
      !If (allocated(sedminps))  deallocate (sedminps)
      !If (allocated(sedorgn))  deallocate (sedorgn)
      !If (allocated(sedorgp))  deallocate (sedorgp)
      !If (allocated(sedyld))  deallocate (sedyld)
      !If (allocated(sanyld))  deallocate (sanyld)
      !If (allocated(silyld))  deallocate (silyld)
      !If (allocated(clayld))  deallocate (clayld)
      !If (allocated(sagyld))  deallocate (sagyld)
      !If (allocated(lagyld))  deallocate (lagyld)
      !If (allocated(grayld))  deallocate (grayld)
      !If (allocated(sed_con))  deallocate (sed_con)
      !If (allocated(sepbtm))  deallocate (sepbtm)
      !If (allocated(smx))  deallocate (smx)
      !If (allocated(snotmp))  deallocate (snotmp)
      !If (allocated(soln_con))  deallocate (soln_con)
      !If (allocated(solp_con))  deallocate (solp_con)
      !If (allocated(stmaxd))  deallocate (stmaxd)
      !If (allocated(itill))  deallocate (itill)
      !If (allocated(surfq))  deallocate (surfq)
      !If (allocated(surqno3))  deallocate (surqno3)
      !If (allocated(surqsolp))  deallocate (surqsolp)
      !If (allocated(swtrg))  deallocate (swtrg)
      !If (allocated(t_ov))  deallocate (t_ov)
      !If (allocated(tconc))  deallocate (tconc)
      !If (allocated(tc_gwat))  deallocate (tc_gwat)
      !If (allocated(tileno3))  deallocate (tileno3)
      !If (allocated(tmn))  deallocate (tmn)
      !If (allocated(tmpav))  deallocate (tmpav)
      !If (allocated(tmx))  deallocate (tmx)
      !If (allocated(twash))  deallocate (twash)
      !If (allocated(u10))  deallocate (u10)
      !If (allocated(usle_cfac))  deallocate (usle_cfac)
      !If (allocated(usle_eifac))  deallocate (usle_eifac)
      !If (allocated(wfsh))  deallocate (wfsh)
      !If (allocated(bss))  deallocate (bss)
      !If (allocated(wrt))  deallocate (wrt)
      !If (allocated(surf_bs))  deallocate (surf_bs)
      !If (allocated(hhsurf_bs))  deallocate (hhsurf_bs)
      !If (allocated(ubnrunoff))  deallocate (ubnrunoff)
      !If (allocated(ubntss))  deallocate (ubntss)
      !If (allocated(hhsedy))  deallocate (hhsedy)
      !If (allocated(hhsurfq))  deallocate (hhsurfq)
      !If (allocated(init_abstrc))  deallocate (init_abstrc)
      !If (allocated(ovrlnd_dt))  deallocate (ovrlnd_dt)
      !If (allocated(tillage_switch))  deallocate (tillage_switch)
      !If (allocated(tillage_depth))  deallocate (tillage_depth)
      !If (allocated(tillage_days))  deallocate (tillage_days)
      !If (allocated(tillage_factor))  deallocate (tillage_factor)

	  
	  
!!    drains
      allocate (wnan(10))
      allocate (ranrns_hru(mhru))
      
      !dimension plant arrays used each day and not saved
       mpc = 10
       allocate (uno3d(mpc))
       allocate (uapd(mpc))
       allocate (un2(mpc))
       allocate (up2(mpc))
       allocate (translt(mpc))
       allocate (par(mpc))
       allocate (htfac(mpc))
       allocate (epmax(mpc))
       epmax = 0.

!!    arrays for plant communities
      allocate (cvm_com(mhru))
      allocate (rsdco_plcom(mhru))
      allocate (percn(mhru))

!! septic changes added 1/28/09 gsm
      allocate (percp(mhru))
      allocate (i_sep(mhru))
      allocate (sep_tsincefail(mhru))
      allocate (qstemm(mhru))
      allocate (bio_bod(mhru))
      allocate (biom(mhru))
      allocate (rbiom(mhru))
      allocate (fcoli(mhru))
      allocate (bz_perc(mhru))
      allocate (plqm(mhru))
      allocate (itb(mhru))
      
      if (time%step > 0) allocate (hhqday(mhru,time%step))
      
 !!  added per JGA for Srini by gsm 9/8/2011
 !! arrays for mangement output (output.mgt)  
      allocate (sol_sumno3(mhru))
      allocate (sol_sumsolp(mhru))

      allocate (iseptic(mhru))

!!    arrays which contain data related to years of rotation,
!!    grazings per year, and HRUs
      allocate (grz_days(mhru))

!!    arrays which contain data related to HRUs
      allocate (brt(mhru))
      allocate (canstor(mhru))
      allocate (cbodu(mhru))
      allocate (chl_a(mhru))
      allocate (cklsp(mhru))
      allocate (cn2(mhru))
      allocate (cnday(mhru))
!    Drainmod tile equations  01/2006 
	  allocate (cumei(mhru))
	  allocate (cumeira(mhru))
	  allocate (cumrt(mhru))
	  allocate (cumrai(mhru))
!    Drainmod tile equations  01/2006
      allocate (dormhr(mhru))
      allocate (doxq(mhru))
      allocate (filterw(mhru))
      allocate (hru_ra(mhru))
      allocate (hru_rmx(mhru))
      allocate (igrz(mhru))
      allocate (yr_skip(mhru))
      allocate (isweep(mhru))
      allocate (phusw(mhru))
      allocate (latno3(mhru))
      allocate (latq(mhru))
      allocate (ndeat(mhru))
      allocate (nplnt(mhru))
      allocate (orgn_con(mhru))
      allocate (orgp_con(mhru))
      allocate (ovrlnd(mhru))
      allocate (phubase(mhru))

      allocate (pplnt(mhru))
      allocate (qdr(mhru))
      allocate (rhd(mhru))

!    Drainmod tile equations  01/2006 
	  allocate (sstmaxd(mhru))	  
!    Drainmod tile equations  01/2006 
      allocate (sedminpa(mhru))
      allocate (sedminps(mhru))
      allocate (sedorgn(mhru))
      allocate (sedorgp(mhru))
      allocate (sedyld(mhru))

      allocate (sanyld(mhru))
      allocate (silyld(mhru))
      allocate (clayld(mhru))
      allocate (sagyld(mhru))
      allocate (lagyld(mhru))
      allocate (grayld(mhru))
      allocate (sed_con(mhru))
      allocate (sepbtm(mhru))
      allocate (smx(mhru))
      allocate (snotmp(mhru))
      allocate (soln_con(mhru))
      allocate (solp_con(mhru))
!!    Drainmod tile equations  01/2006 
	  allocate (stmaxd(mhru))
      allocate (itill(mhru))
      allocate (surfq(mhru))
      allocate (surqno3(mhru))
      allocate (surqsolp(mhru))
      allocate (swtrg(mhru))
      allocate (t_ov(mhru))
      allocate (tconc(mhru))
      allocate (tc_gwat(mhru))
      allocate (tileno3(mhru))
      allocate (tmn(mhru))
      allocate (tmpav(mhru))
      allocate (tmx(mhru))
      allocate (twash(mhru))
      allocate (u10(mhru))
      allocate (usle_cfac(mhru))
      allocate (usle_eifac(mhru))
      allocate (wfsh(mhru))

      allocate (bss(4,mhru))
      allocate (wrt(2,mhru))
      allocate (surf_bs(17,mhru))  

!! sj aug 09 end
	  allocate (hhsurf_bs(2,mhru,time%step))
      allocate (ubnrunoff(time%step),ubntss(time%step))

!! Arrays for subdaily erosion modeling by Jaehak Jeong
	  allocate (hhsedy(mhru,time%step),ovrlnd_dt(mhru,time%step))  
	  allocate (init_abstrc(mhru),hhsurfq(mhru,time%step))

       !Tillage factor on SOM decomposition
       allocate(tillage_switch(mhru))
       allocate(tillage_depth(mhru))
       allocate(tillage_days(mhru))
       allocate(tillage_factor(mhru))
       
       tillage_switch = 0
       tillage_depth = 0.
       tillage_days = 0
       tillage_factor = 0.
       
      !! By Zhang for C/N cycling
      !! ============================
      	  
      call zero0
      call zero1
      call zero2
      call zeroini

!!    zero reservoir module
      return
      end