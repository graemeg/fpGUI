package net.miginfocom.layout;

import net.miginfocom.layout.UnitValue;
import net.miginfocom.layout.ComponentWrapper;
import net.miginfocom.layout.BoundSize;

/** A class to extend if you want to provide more control over where a component is placed or the size of it.
 * <p>
 * Note! Returned arrays from this class will never be altered. This means that caching of arrays in these methods
 * is OK.
 */
public abstract class LayoutCallback
{
	/** Returns a position similar to the "pos" the component constraint.
	 * @param comp The component wrapper that holds the actual component (JComponent is Swing and Control in SWT).
	 * <b>Should not be altered.</b>
	 * @return The [x, y, x2, y2] as explained in the documentation for "pos". If <code>null</code>
	 * is returned nothing is done and this is the default.
	 * @see UnitValue
	 * @see net.miginfocom.layout.ConstraintParser#parseUnitValue(String, boolean)
	 */
	public UnitValue[] getPosition(ComponentWrapper comp)
	{
		return null;
	}

	/** Returns a size similar to the "width" and "height" in the component constraint.
	 * @param comp The component wrapper that holds the actual component (JComponent is Swing and Control in SWT).
	 * <b>Should not be altered.</b>
	 * @return The [width, height] as explained in the documentation for "width" and "height". If <code>null</code>
	 * is returned nothing is done and this is the default.
	 * @see net.miginfocom.layout.BoundSize
	 * @see net.miginfocom.layout.ConstraintParser#parseBoundSize(String, boolean, boolean)
	 */
	public BoundSize[] getSize(ComponentWrapper comp)
	{
		return null;
	}

	/** A last minute change of the bounds. The bound for the layout cycle has been set and you can correct there
	 * after any set of rules you like.
	 * @param comp The component wrapper that holds the actual component (JComponent is Swing and Control in SWT).
	 * <b>Should not be altered.</b>
	 */
	public void correctBounds(ComponentWrapper comp)
	{
	}
}
