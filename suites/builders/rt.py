""" GEFF Ensemble Forecast suite """

from comfies.sdeploy import BaseBuilder
from comfies import config
from comfies.ooflow import Task, Family, RepeatDate, Variable
from comfies.ooflow import Trigger, Defuser, Defstatus, complete
from suites.parts.idioms import DummyFamily
from suites.parts.epilogs import DummyEpilog
from suites.parts.times import Timer, CronDateRefresh
from comfies.dateandtime import Date



class Builder(BaseBuilder):


    scripts = [
            'suites/scripts/common',
            'suites/scripts/rt',
            ]

    includes = [
            'suites/scripts/common',
            'suites/scripts/rt',
            ]


    def build(self):

        cfg = self.config
        # get initial dates and other stuff from the deploy config file
        first_fillup  = cfg.get('first_fillup')
        first_fc      = cfg.get('first_fc')
        last_date     = cfg.get('last_date', default='20200101')
        first_barrier = cfg.get('first_barrier', default=last_date)
        with_ens      = cfg.get('with_ens', type=config.boolean, default=True)
        jobs_limit    = cfg.get('jobs.limit', type=int, default=10)
        mars_nworkers = cfg.get('mars_nworkers', type=int, default=1)
        with_diss     = cfg.get('with_diss', type=config.boolean, default=False)
        follow_osuite = cfg.get('follow_osuite', type=config.boolean, default=False)
        use_latest_reanalysis = cfg.get('use_latest_reanalysis', type=config.boolean, default=False)

        self.suite.add_limit('fillup', 7)
        self.suite.add_limit('forecast', jobs_limit)
        self.suite.add_limit('lag_fillup', 4)
        self.suite.add_limit('lag_forecast', 4)
        self.suite.add_variable('COLD_START', 0)
        self.suite.add_variable('hres_resol', '')
        self.suite.add_variable('ens_resol', '')

        # -----------------------------------------------------------
        # GEFF RT fillup run uses latest GEFF RE output as initial
        # conditions. GEFF RE has a top-level variable which indicates
        # what is the most recent ERA-INTERIM Fire model run.
        # -----------------------------------------------------------

        geff_re_latest_str = '/fire_ei_ws:latest'
        self.defs.add_extern(geff_re_latest_str)

        # -----------------------------------------------------------
        # 'setup' family - initialization of the newly created suite.
        # -----------------------------------------------------------

        n_build = Family('build')
        n_build.add_task('build_geff')
        n_build.add_task('install_scripts')

        n_setup = Family('setup')
        n_setup.add(n_build)

        # -----------------------------------------------------------
        # Barrier family. GEFF RT model run follows it.
        # Barrier advances once per day. It follows 00 cycle of /mc
        # suite (either has a timer or external dependency on /mc).
        # -----------------------------------------------------------

        if follow_osuite:
            # Start as soon as ECMWF ensemble forecast finishes.
            # '/mc' suite must be visible (on the same server).
            self.defs.add_extern('/mc/main:YMD')
            self.defs.add_extern('/mc/main/00/fc0015d/fc')
            self.defs.add_extern('/o/main:YMD')
            self.defs.add_extern('/o/main/00/fc/model')
            n_run_hr = Task('run_hr').add(
                    Defstatus(complete))
            n_run_en = Family('run_en').add(
                Trigger('(cal::date_to_julian(/mc/main:YMD)+1 > cal::date_to_julian(/{s}/barrier:YMD) or '
                        'cal::date_to_julian(/mc/main:YMD)+1 == cal::date_to_julian(/{s}/barrier:YMD) and '
                        '/mc/main/00/fc0015d/fc == complete)'.format(s=cfg.get('name'))))
            n_run_en.add(Task('dummy').add(Trigger('0==1')).add(Defuser('1==1')))
            n_barrier = Family('barrier')
            n_barrier.add(n_run_hr, n_run_en)
            n_barrier.add(
                Family('last').add(
                    Trigger('(/o/main:YMD > /{s}/barrier:YMD or '
                        '/o/main:YMD == /{s}/barrier:YMD and '
                        '/o/main/00/fc/model == complete) and '
                        '/{s}/barrier/run_en == complete'.format(s=cfg.get('name'))),
                    Task('sleep').add(
                        Trigger('0==1'),
                        Defuser('1==1'))))
            barrier_ymd = RepeatDate('YMD', int(first_barrier), int(last_date))
            n_barrier.add(barrier_ymd)
        else:
            # Start at fixed time
            n_barrier = CronDateRefresh(
                    'barrier',
                    time = '10:30',
                    base = Date.from_ymd(first_barrier),
                    base_shift = 0)
            barrier_ymd = n_barrier.ymd

        # 'ic_ymd' and 'start_ymd' are global (suite-level) variables;
        # they are updated at each cycle by 'fillup_rewind' task.
        # ic_ymd: what is the most recent restart file
        # start_ymd: start of the fillup (or forecast if no fillup)

        ic_ymd = Variable('ic_ymd', '19010101')
        # 'latest' is for production suite (pretend to be renalysis suite)
        latest = Variable('latest', '19010101')
        self.suite.add(ic_ymd, latest)
        self.suite.add_label('ic_ymd', '19010101')

        start_ymd = Variable('start_ymd', '19010102')
        self.suite.add_variable(start_ymd)
        self.suite.add_label('start_ymd', '19010102')

        # Forecast YMD repeat and fillup_rewind task: these need to
        # be defined here because 'fillup_forcings' and 'fillup' families
        # refer to them.

        forecast_ymd = RepeatDate('YMD', int(first_fc), int(last_date))
        n_fillup_rewind = Task('fillup_rewind')
        n_fillup_rewind.trigger = forecast_ymd <= barrier_ymd
        n_fillup_rewind.trigger &= n_build.complete
        n_fillup_rewind.add_label('info', '')

        # -----------------------------------------------------------
        # GEFF RT fillup computations
        # -----------------------------------------------------------

        # YMD repeat which runs fillup simulation.
        # We rewind this YMD when there is new GEFF-RE output
        # and re-do fillup simulation starting from the
        # GEFF-RE and ending where forecast begins.

        n_fillup = Family('fillup')
        n_fillup.add_variable('CONTEXT', 'fillup')
        n_fillup.add_inlimit('fillup')
        fillup_ymd = RepeatDate('YMD', int(first_fillup), int(last_date))
        n_fillup.add(fillup_ymd)

        n_fillup_do = Family('do')
        n_fillup_do.trigger = n_fillup_rewind.complete.across('YMD') & (start_ymd < fillup_ymd)
        n_fillup_do.trigger &= ~n_fillup_rewind.active & (ic_ymd > 19020101)
        # do not run fillup if there is a restart file for current date
        n_fillup_do.defuser = n_fillup_rewind.complete.across('YMD') & (start_ymd >= fillup_ymd)

        n_fillup_init = Family('init')
        n_fillup_const_prep = Task('const_prep')
        n_fillup_marsreq = Task('fillup_marsreq')
        n_fillup_init.add(n_fillup_const_prep, n_fillup_marsreq)

        n_fillup_forcings = Family('forcings')
        n_fillup_forcings.trigger = n_fillup_init.complete
        for param in 'ws', 'rh', 'tt', 'pr', 'sc', 'cc':
            n_fillup_forcings.add_task(param)

        n_fillup_ic = Task('fillup_ic')
        n_fillup_ic.trigger = n_fillup_const_prep.complete
        n_fillup_ecfire = Task('ecfire')
        n_fillup_ecfire.trigger = n_fillup_ic.complete & n_fillup_forcings.complete

        n_fillup_do.add(n_fillup_init, n_fillup_forcings, n_fillup_ic, n_fillup_ecfire)
        n_fillup.add(n_fillup_do)
        n_fillup.add(DummyEpilog(done = forecast_ymd > fillup_ymd))

        # -----------------------------------------------------------
        # GEFF RT main computations
        # -----------------------------------------------------------

        n_forecast = Family('forecast')
        n_forecast.add_variable('CONTEXT', 'forecast')
        n_forecast.add_inlimit('forecast')

        # fillup_rewind - initialize /geff_rt/fillup_forcings
        # and /geff_rt/fillup families for computing fillup
        n_forecast.add(n_fillup_rewind)


        n_fc = Family('fc')
        n_fc.trigger = n_fillup_do.complete.across('YMD')

        # Deterministic

        def sim_family(name):
            n_sim = Family(name)
            n_sim.add_variable('ENS_MEMBER', name)
            n_forcings = Family('forcings')
            for param in ['fc_ws', 'fc_rh', 'fc_tt', 'fc_pr', 'fc_sc', 'fc_cc']:
                n_forcings.add_task(param)
            n_sim.add(n_forcings)
            n_ecfire = Task('ecfire')
            n_ecfire.trigger = n_forcings.complete
            n_sim.add(n_ecfire)
            return n_sim

        n_hres = Family('hres')
        n_hres.add_variable('CONTEXT', 'hres')
        n_hres.add_variable('FCTYPE', 'hr')
        n_hres_init = Family('init')
        n_hres_init.add(
                Task('const_prep'),
                Task('hres_ic').add_trigger('const_prep == complete'),
                Task('fc_hres_marsreq'))
        n_hres.add(n_hres_init)
        n_hres_fc = sim_family('hr')
        n_hres_fc.trigger = n_hres_init.complete
        n_hres.add(n_hres_fc)
        n_hres_tar = Task('fc_tar')
        n_hres_tar.trigger = n_hres_fc.complete
        n_hres.add(n_hres_tar)
        if with_diss:
            n_hres_diss = Task('fc_diss')
            n_hres_diss.trigger = n_hres_tar.complete
            n_hres.add(n_hres_diss)
        n_fc.add(n_hres)

        # Ensemble

        n_ens = Family('ens')
        if follow_osuite:
            n_ens.trigger = n_run_en.complete.across('YMD')
        n_ens.add_variable('CONTEXT', 'ens')
        n_ens.add_variable('FCTYPE', 'en')
        # const fields, initial conditions and forcings for ensemble
        n_ens_init = Family('init')
        n_ens_init.add(
                Task('const_prep'),
                Task('ens_ic').add_trigger('const_prep == complete'))
        n_ens_mars = Family('mars')
        n_ens_mars.add_variable('NWORKERS', mars_nworkers)
        for worker in range(1, mars_nworkers+1):
            n_worker = Family(str(worker))
            n_worker.add_variable('WORKER', worker)
            n_worker.add_task('fc_ens_marsreq')
            n_ens_mars.add(n_worker)
        if with_ens:
            n_ens_init.add(n_ens_mars)
        n_ens.add(n_ens_init)
        # model ensemble
        n_ens_ens = Family('ens')
        n_ens_ens.trigger = n_ens_init.complete
        n_ens.add(n_ens_ens)
        # create tar for archiving and dissemination
        n_ens_tar = Task('fc_tar')
        n_ens_tar.trigger = n_ens_ens.complete
        n_ens.add(n_ens_tar)
        if with_diss:
            # dissemination
            n_ens_diss = Task('fc_diss')
            n_ens_diss.trigger = n_ens_tar.complete
            n_ens.add(n_ens_diss)
        for member in [str(x).zfill(2) for x in range(0, 51)]:
            n_mem = sim_family(member)
            n_ens_ens.add(n_mem)
        if with_ens:
            n_fc.add(n_ens)

        n_forecast_epilog = DummyEpilog(done = barrier_ymd > forecast_ymd)

        # create tar files for archiving and dissemination

        n_forecast.add(forecast_ymd, n_fc)


        n_forecast.add(n_forecast_epilog)

        self.suite.add(n_setup, n_barrier, n_fillup, n_forecast)


        # -----------------------------------------
        # GEFF-RT cleaning, archiving
        # -----------------------------------------

        n_lag = Family('lag')

        # cleaning of fillup

        n_fillup_lag = Family('fillup')
        n_fillup_lag.add_inlimit('lag_fillup')
        fillup_lag_ymd = RepeatDate('YMD', int(first_fillup), int(last_date))
        n_fillup_lag.add(fillup_lag_ymd)
        n_fillup_clean = Task('fillup_clean')
        n_fillup_lag.add(n_fillup_clean)
        fillup_lag_done = fillup_lag_ymd < fillup_ymd
        n_fillup_lag.add(DummyEpilog(done = fillup_lag_done))
        n_lag.add(n_fillup_lag)

        # archiving and cleaning of forecast

        n_forecast_lag = Family('forecast')
        n_forecast_lag.add_inlimit('lag_forecast')
        forecast_lag_ymd = RepeatDate('YMD', int(first_fc), int(last_date))
        n_forecast_lag.add(forecast_lag_ymd)
        n_forecast_lag.add_task('fc_clean')

        def fctype_arch(fctype):
            n_fctype = Family(fctype)
            n_fctype.add_variable('FCTYPE', fctype)
            n_fctype.add_task('fc_arch')
            return n_fctype

        n_fc_arch = Family('archive')
        n_fc_arch.add(fctype_arch('hr'))
        if with_ens:
            n_fc_arch.add(fctype_arch('en'))
        n_fc_arch.trigger =  n_fc.complete.across('YMD')
        n_forecast_lag.add(n_fc_arch)

        n_forecast_lag.add(DummyEpilog(done = forecast_ymd > forecast_lag_ymd))
        n_lag.add(n_forecast_lag)

        self.suite.add(n_lag)
