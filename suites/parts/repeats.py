from comfies.ooflow import RepeatEnumerated
from comfies.dateandtime import Date, calseq, DAY


_distant_future = Date(2022, 1, 1)


def calseq_repeat(start, stop=_distant_future,
                  weekdays=[], monthdays=[],
                  months=range(1,13)):
    """
    Sequence of arbitrary dates defined in the same
    way as for comfies.dateandtime.calseq() function
    """
    dates = calseq(start, stop+DAY, weekdays=weekdays,
                   monthdays=monthdays, months=months)
    return RepeatEnumerated('YMD', [str(d.ymd) for d in dates])
