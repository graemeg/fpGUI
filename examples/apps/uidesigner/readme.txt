
   fpGUI's Visual User Interface Designer
   ======================================


Introduction
============

fpGUI Designer is a port of a product called VFD written by Nagy Viktor in 2003.
I ported VFD to fpGUI because I thought it would add a lot to the framework as 
a whole.  I could see it has lots of potential and could easily be extended to 
become a great tool. What I immediately liked about VFD is that it used the 
same source unit and writes actual code describing the UI like I would have 
done manually. This departs from the GUI designers used in Lazarus or Delphi 
where they save the UI code to external files (*.lfm or *.dfm respectively).


Features
========

* It uses the same source unit where your normal code lives. No external GUI 
  files.

* It can handle "unknown" or "custom" components and properties. Whenever the 
  designer finds a component it doesn't natively know it paints a green 
  rectangle in it's place. Whenever it finds properties it doesn't know it 
  adds them "as is" to the Unknown section memo in the Properties window. Any 
  code in the Unknown memo will be written back to the source unit "as is".

* The designer can handle more that one form in a unit.

* When creating a new form, that form can be merged into an existing unit or
  the designer will create a new unit for it.

* The designer uses comment markers in the code to define what parts it will
  maintain. Existing code (Forms) can be easily amended (manually by the 
  programmer) so that the designer can start managing it. No need to redesign
  Forms from scratch.



Graeme Geldenhuys - September 2007

                ============================================



  


