
  Model-GUI-Mediator (MGM) pattern implementation by Graeme Geldenhuys.
  ---------------------------------------------------------------------

This allows standard controls or any other GUI controls for that matter to become
object-aware.  The benefits are much greater ease in cross platform development, 
or if you would like to make your favorite set of GUI controls object-aware.

Please note this is work in progress...
So far I have implemented most used basic edit controls:
     TfpgEdit
     TfpgSpinEdit    (dependend on fpGUI component)
     TfpgLabel
     TfpgComboBox    (single and list property)
     TfpgTrackBar
     TfpgMemo
     TfpgListView    (includes popup menu)
     TfpgListBox     (includes popup menu)

Currently I am considering implementing the container classes like TfpgTreeView.
They are quite complex and there are multiple ways of implementing them.

For more information on the MGM pattern, visit Andy Bulka's website.
  http://www.atug.com/andypatterns/mgm.htm


                              ----oO0Oo----


