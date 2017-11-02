from comfies.ooflow import Family, Trigger, all_complete
from comfies.ooflow import Bool, NullTrigger


class DummyEpilog(Family):

    """
    When added to a family, it will run at the end
    (waits for all children of the parent family
    and for the 'done' trigger/timer).
    Does not do anything, just sleeps for a few seconds.
    """

    def __init__(self, name='last', done=NullTrigger()):
        super(DummyEpilog, self).__init__(name)
        self.add_task('dummy')
        self.add_variable('SLEEP', 3)
        if isinstance(done, Bool):
            done = Trigger(done)
        self.add(done)

    def add_to(self, node):
        self.trigger &= all_complete(node.children)
        super(DummyEpilog, self).add_to(node)
