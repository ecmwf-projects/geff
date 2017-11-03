""" GEFF Ensemble Forecast suite """

from comfies.sdeploy import BaseBuilder
from comfies import config
from comfies.ooflow import Task, Family, RepeatDate, Variable
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

        self.suite.add_limit('limit', jobs_limit)
        self.suite.add_inlimit('limit')
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
        self.suite.add(ic_ymd)
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



        # -----------------------------------------------------------
        # GEFF RT fillup computations
        # -----------------------------------------------------------


        # YMD repeat which prepares fillup forcings

        n_fforc = Family('fillup_forcings')
        n_fforc.add_variable('CONTEXT', 'fillup')
        fforc_ymd = RepeatDate('YMD', int(first_fillup), int(last_date))
        n_fforc.add(fforc_ymd)

        n_fforc_do = Family('do')
        n_fforc_do.trigger = n_fillup_rewind.complete.across('YMD') & (start_ymd < fforc_ymd)
        n_fforc_do.trigger &= ~n_fillup_rewind.active & (ic_ymd > 19020101)
        # do not run fillup if there is a restart file for current date
        n_fforc_do.defuser = n_fillup_rewind.complete.across('YMD') & (start_ymd >= fforc_ymd)

        n_fforc_init = Family('init')
        n_fforc_init.add(Task('fillup_marsreq'), Task('const_prep'))

        n_fforc_forcings = Family('forcings')
        n_fforc_forcings.trigger = n_fforc_init.complete
        for param in 'ws', 'rh', 'tt', 'pr', 'sc', 'cc':
            n_fforc_forcings.add_task(param)

        n_fforc_do.add(n_fforc_init, n_fforc_forcings)

        n_fforc.add(n_fforc_do)
        #n_fforc.add(DummyEpilog(done = init_ymd > fforc_ymd))
        n_fforc.add(DummyEpilog(done = forecast_ymd > fforc_ymd))

        # YMD repeat which runs fillup simulation.
        # We rewind this YMD when there is new GEFF-RE output
        # and re-do fillup simulation starting from the
        # GEFF-RE and ending where forecast begins.

        n_fillup = Family('fillup')
        n_fillup.add_variable('CONTEXT', 'fillup')
        fillup_ymd = RepeatDate('YMD', int(first_fillup), int(last_date))
        n_fillup.add(fillup_ymd)

        n_fillup_do = Family('do')
        n_fillup_do.trigger = n_fforc_do.complete.across('YMD') & (start_ymd < fillup_ymd)
        n_fillup_do.defuser = n_fforc_do.complete.across('YMD') & (start_ymd >= fillup_ymd)

        n_fillup_ic = Task('fillup_ic')

        n_fillup_ecfire = Task('ecfire')
        n_fillup_ecfire.trigger = n_fillup_ic.complete

        n_fillup_do.add(n_fillup_ic, n_fillup_ecfire)
        n_fillup.add(n_fillup_do)
        n_fillup.add(DummyEpilog(done = fforc_ymd > fillup_ymd))

        # -----------------------------------------------------------
        # GEFF RT main computations
        # -----------------------------------------------------------

        n_forecast = Family('forecast')
        n_forecast.add_variable('CONTEXT', 'forecast')

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
            n_ecfire = Task('ecfire')
            n_ecfire.trigger = n_forcings.complete
            n_sim.add(n_forcings, n_ecfire)
            return n_sim

        n_hres = Family('hres')
        n_hres.add_variable('CONTEXT', 'hres')
        n_hres_init = Family('init')
        n_hres_init.add(
                Task('const_prep'),
                Task('hres_ic').add_trigger('const_prep == complete'),
                Task('fc_hres_marsreq'))
        n_hres_fc = sim_family('hr')
        n_hres_fc.trigger = n_hres_init.complete
        n_hres.add(n_hres_init, n_hres_fc)
        n_fc.add(n_hres)

        # Ensemble

        n_ens = Family('ens')
        n_ens.add_variable('CONTEXT', 'ens')
        # const fields, initial conditions and forcings for ensemble
        n_ens_init = Family('init')
        n_ens_init.add(
                Task('const_prep'),
                Task('ens_ic').add_trigger('const_prep == complete'))
        n_ens_mars = Family('mars')
        n_ens_mars.add_variable('NWORKERS', 4)
        for worker in range(1, 5):
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
        for member in [str(x).zfill(2) for x in range(0, 51)]:
            n_mem = sim_family(member)
            n_ens_ens.add(n_mem)
        if with_ens:
            n_fc.add(n_ens)

        n_forecast_epilog = DummyEpilog(done = barrier_ymd > forecast_ymd)
        n_forecast.add(forecast_ymd, n_fc, n_forecast_epilog)

        self.suite.add(n_setup, n_barrier, n_fforc, n_fillup, n_forecast)


        # -----------------------------------------
        # GEFF-RT cleaning, archiving
        # -----------------------------------------

        n_lag = Family('lag')

        # cleaning of fillup

        n_fillup_lag = Family('fillup')
        fillup_lag_ymd = RepeatDate('YMD', int(first_fillup), int(last_date))
        n_fillup_lag.add(fillup_lag_ymd)
        n_fillup_clean = Task('fillup_clean')
        n_fillup_lag.add(n_fillup_clean)
        n_fillup_lag.add(DummyEpilog(done = start_ymd > fillup_lag_ymd))
        n_lag.add(n_fillup_lag)

        # archiving and cleaning of forecast

        n_forecast_lag = Family('forecast')
        forecast_lag_ymd = RepeatDate('YMD', int(first_fc), int(last_date))
        n_forecast_lag.add(forecast_lag_ymd)
        n_forecast_lag.add_task('fc_clean')

        n_fc_arch = Family('archive')
        n_forecast_lag.add(n_fc_arch)

        for fctype, trigger in (('hr', n_hres.complete),
                                ('en',  n_ens.complete)):
            n_fctype = Family(fctype)
            n_fctype.trigger = trigger
            n_fctype.add_variable('FCTYPE', fctype)
            n_fc_arch.add(n_fctype)
            for subset in ('mark5', 'nfdrs', 'cfwis'):
                n_subset = Family(subset)
                n_subset.add_task('fc_arch')
                n_subset.add_variable('SUBSET', subset)
                n_fctype.add(n_subset)

        n_forecast_lag.add(DummyEpilog(done = forecast_ymd > forecast_lag_ymd))
        n_lag.add(n_forecast_lag)

        self.suite.add(n_lag)
