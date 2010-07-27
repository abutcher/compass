#!/opt/local/bin/python
"""
Yeah, this is a comment.
"""
__author__="""Andrew Butcher (abutcher@csee.wvu.edu)"""

import arff
import csv
import compass
import warnings
from util import *
import wx

try:
    import matplotlib.pyplot as plt
except:
    raise

import networkx as nx

warnings.simplefilter('ignore', DeprecationWarning)

class compassexplorer:
    def __init__(self, func, *args, **kwargs):
        self.func = func
        self.pending = args[:]
        self.kwargs = kwargs
    def __call__(self, *args, **kwargs):
        if kwargs and self.kwargs:
            kw = self.kwargs.copy()
            kw.update(kwargs)
        else:
            kw = kwargs or self.kwargs
        return self.func(*(self.pending + args), **kw)

class MainFrame(wx.Frame):
    def BuildSubmenu(self, subMenu):
        subMenuObject = wx.Menu()
        for item in subMenu:
            if not item:
                subMenuObject.AppendSeparator()
                continue
            statustext = ''; uihandler = None
            if len(item) == 2:
                title, action = item
            elif len(item) == 3:
                if type(item[2]) is str:
                    title, action, statustext = item
                else:
                    title, action, statustext = item
            elif len(item) == 4:
                title, action, statustext, uihandler = item
            else:
                raise AssertionError, \
                    'Item %s should have either 2 to 4 parts' % (item,)
            if type(action) is list:
                _id = wx.NewId()
                subMenuObject.AppendMenu(_id, title, self.BuildSubmenu(action))
            else:
                _id = wx.NewId()
                subMenuObject.Append(_id, title, statustext)
                wx.EVT_MENU(self, _id, action)
            if uihandler:
                wx.EVT_UPDATE_UI(self, _id, uihandler)
        return subMenuObject

    def BuildMenu(self, menu):
        mainMenu = wx.MenuBar()
        for title, subMenu in menu:
            mainMenu.Append(self.BuildSubmenu(subMenu), title)
        return mainMenu

    def MyMenu(self):
        menu = [
            ('&File', [
                ('&Open', self.OpenFile),
            ])
        ]
        self.SetMenuBar(self.BuildMenu(menu))

    def BuildTextBox(self):
        self.textbox = wx.TextCtrl(
            self, -1, "", 
            wx.Point(0,0), wx.DefaultSize, 
            wx.TE_MULTILINE
            )
  
        topsizer = wx.BoxSizer(wx.HORIZONTAL)
#        menusizer = wx.BoxSizer(wx.VERTICAL)        
        contentsizer = wx.BoxSizer(wx.VERTICAL)

        contentsizer.Add(self.textbox, 1, wx.EXPAND)
        topsizer.Add(contentsizer, 1, wx.EXPAND)

        def walk(node, level=0):
            newid = wx.NewId()
            self.textbox.AppendText(
                "%sLevel: %d, Size: %d, Variance: %6.2f\n" % ("\t"*level, level, len(node.data), node.variance))
            if node.left != None:
                walk(node.left, level + 1)
            if node.right != None:
                walk(node.right, level + 1)
                
        myarff=arff.Arff("/Users/abutcher/compass/src/python/arff/telecom1.arff")
        compasstree=compass.CompassTree(myarff.data)
        walk(compasstree.root)

    def __init__(self, parent, id):
        wx.Frame.__init__(self, parent, id, 'Compass Explorer')
        self.CreateStatusBar()
        self.SetStatusText('West Virginia University')
        self.MyMenu()
        self.BuildTextBox()

    def OpenFile(self, event):
        dlg = wx.FileDialog(self, "Choose a file", os.getcwd(), "", "*.*", wx.OPEN)
        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            mypath = os.path.basename(path)
            self.SetStatusText("You selected: %s" % mypath)
        dlg.Destroy()

class MyApp(wx.App):
    def OnInit(self):
        frame = MainFrame(None, -1)
        frame.Show(True)
        frame.SetSize(wx.Size(750,550))
        self.SetTopWindow(frame)
        return True
        
if __name__ == '__main__':
    app = MyApp(0)
    app.MainLoop()
