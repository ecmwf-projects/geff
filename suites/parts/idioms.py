from comfies.ooflow import Family, Task


class DummyTask(Task):
    def __init__(self, name):
        super(DummyTask, self).__init__(name)
        self.trigger = '0==1'
        self.defuser = '1==1'


class DummyFamily(Family):
    def __init__(self, name='dummy'):
        super(DummyFamily, self).__init__(name)
        n_dummy = Task('dummy')
        n_dummy.trigger = '0==1'
        n_dummy.defuser = '1==1'
        self.add(n_dummy)
