from comfies.ooflow import Family, Task, Variable
from comfies.dateandtime import Date, DAY, Time
from comfies.dateandtime import CalSeq, future
import datetime
import ecflow


class CronDateRefresh(Family):

    def __init__(
            self,
            name,
            time=None,
            weekdays=None,
            monthdays=None,
            months=None,
            base_shift=0,
            seq_shift=0,
            base=Date.from_ymd('19500101')
            ):

        super(CronDateRefresh, self).__init__(name)

        t = Task('ymd')
        t.add(Variable('base_shift', base_shift))
        t.add(Variable('seq_shift', seq_shift))

        seq_pattern=''
        if weekdays is not None:
            weekdays_str = ','.join([str(x) for x in weekdays])
            t.add(Variable('weekdays', weekdays_str))
            seq_pattern += ' -w' + weekdays_str
        if monthdays is not None:
            monthdays_str = ','.join([str(x) for x in monthdays])
            t.add(Variable('monthdays', monthdays_str))
            seq_pattern += ' -d' + monthdays_str
        if months is not None:
            months_str = ','.join([str(x) for x in months])
            t.add(Variable('months', months_str))
            seq_pattern += ' -m' + months_str

        # calculate initial YMD value

        shifted_base = base + (base_shift * DAY)
        #self.ymd = Variable('YMD', sh.ymd)
        #t.add(self.ymd)

        calseq = CalSeq(
                weekdays=weekdays,
                monthdays=monthdays,
                months=months
                )
        # normalize shifted_base to the most recent date from the sequence
        seq_date = calseq.lsnap(shifted_base)

        # finally, apply seq_shift and set the attached YMD variable
        shifted_seq_date = calseq.shift(seq_date, seq_shift)
        self.ymd = Variable('YMD', shifted_seq_date.ymd)
        t.add(self.ymd)

        # display some info
        t.add_label('base', base.ymd)
        t.add_label('base_shift', str(base_shift))
        t.add_label('seq_pattern', seq_pattern)
        t.add_label('seq_shift', str(seq_shift))
        t.add_label('YMD', shifted_seq_date.ymd)

        # task will be executed once a day by attached cron
        cron = ecflow.Cron()
        cron.set_time_series(str(time))
        t.add_cron(cron)
        self.add(t)



def t2t(t):
    """
    Given ecflow.Time, return first ecflow.TimeSlot as datetime.time object.
    Given ecflow.TimeSeries, return first ecflow.TimeSlot as a datetime.time object.
    Given ecflow.TimeSlot, return a datetime.time object.
    Given datetime.time object, return ecflow.Time object.
    """
    if isinstance(t, ecflow.TimeSlot):
        return datetime.time(hour = t.hour(), minute = t.minute())
    if isinstance(t, ecflow.Time):
        return t2t(t.time_series().start())
    elif isinstance(t, ecflow.TimeSeries):
        return t2t(t.start())
    elif isinstance(t, datetime.time):
        return ecflow.Time(t.hour, t.minute)
    else:
        raise TypeError('expected ecflow.TimeSlot, ecflow.Time, ecflow.TimeSeries or datetime.time, not {}'.format(type(t)))



class Timer(object):

    def __init__(self, time):
        if isinstance(time, Time):
            time = str(time)
        if isinstance(time, datetime.time):
            time = '{02d}:{02d}'.format(time.hours, time.minutes)
        self.time = time

    def __str__(self):
        return str(self.time)

    def add_to(self, node):
        node.add_time(self.time)
