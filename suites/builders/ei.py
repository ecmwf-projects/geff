""" GEFF ERA INTERIM suite """

from comfies.dateandtime import Date, TimeDelta
from comfies.ooflow import Family, Task, Variable
from comfies.ooflow import RepeatDate, Label
from comfies.sdeploy import BaseBuilder
from suites.parts.times import CronDateRefresh
from suites.parts.epilogs import DummyEpilog
from suites.parts.repeats import calseq_repeat


class Builder(BaseBuilder):

    scripts = [
        'suites/scripts/ei',
        'suites/scripts/common'
        ]

    includes = [
        'suites/scripts/ei',
        'suites/scripts/common'
        ]

    def build(self):

        cfg = self.config
        first_date = Date.from_ymd(cfg.get('first_date'))
        first_ei_date = first_date - TimeDelta(days=1)
        last_date = Date.from_ymd(cfg.get('last_date', '20220101'))

        # Timer triggering era interim mars retrieval loop step.
        n_ei_start = CronDateRefresh(
                'start',
                time = '15:00',
                base = first_date,
                base_shift = -3)
        self.suite.add(Family('timers').add(n_ei_start))

        self.suite.add(Variable('COLD_START', 0))

        # the ':latest' variable/label shows the most recent
        # fire danger computed. External suites (eg. GEFF-RT)
        # refer to this variable when they need to know what
        # is the most recent GEFF_RE output.

        self.suite.add_variable('latest', '0')
        self.suite.add_label('latest', '0')

        # 'ei' family retrieves one month of Era Interim from Mars
        # It runs 1 day ahead of 'fire' family.

        n_ei = Family('ei')
        #ei_ymd = RepeatDate('YMD', int(first_ei_date.ymd), int(last_date.ymd))
        ei_ymd = calseq_repeat(first_ei_date, monthdays=[1])
        fire_ymd = RepeatDate('YMD', int(first_date.ymd), int(last_date.ymd))
        n_ei.add(ei_ymd)
        # the task that retrieves one day of Era Interim
        n_ei_get_nwp = Task('ei_get_nwp')
        n_ei_get_nwp.trigger = n_ei_start.ymd >= ei_ymd
        # FIRE_YMD variable says which fire date can be run.
        # Fire model runs 1 day behind 'ei_get_nwp'.
        ei_fire_ymd = Variable('FIRE_YMD', 19000101)
        self.suite.add(ei_fire_ymd)
        self.suite.add(Label('FIRE_YMD', ''))
        n_ei.add(n_ei_get_nwp)
        n_ei_epilog = DummyEpilog(done = (n_ei_start.ymd > ei_ymd) & (ei_ymd <= fire_ymd + 2))
        n_ei.add(n_ei_epilog)
        self.suite.add(n_ei)


        # 'fire' family computes one day of fire model.
        # It runs 1 day behind 'ei' family.

        n_fire = Family('fire')
        n_fire.add_repeat(fire_ymd)
        n_ic = Task('ic')
        n_forcings = Family('forcings')
        n_forcings.trigger = ei_fire_ymd >= fire_ymd
        for param in 'ws', 'rh', 'tt', 'pr', 'sc', 'cc', 'lm', 'slope', 'vc', 'vs':
            n_forcings.add_task(param)
        n_fire.add(n_ic, n_forcings)
        n_ecfire = Task('ecfire')
        n_ecfire.trigger = n_ic.complete & n_forcings.complete
        # 'update_latest' task updates the value of '/geff_re:latest' variable
        n_update_latest = Task('update_latest')
        n_update_latest.trigger = n_ecfire.complete
        n_ei_clean = Task('ei_clean')
        n_ei_clean.trigger = n_ecfire.complete
        n_ei_archive = Task('ei_archive')
        n_ei_archive.trigger = n_ecfire.complete
        n_fire.add(n_ecfire, n_update_latest, n_ei_archive, n_ei_clean)
        n_fire_epilog = DummyEpilog(done = ei_fire_ymd > fire_ymd)
        n_fire.add(n_fire_epilog)
        self.suite.add(n_fire)
