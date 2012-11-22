package net.miginfocom.layout;

/** An interfance to implement if you want to decide the gaps between two types of components within the same cell.
 * <p>
 * E.g.:
 *
 * <pre>
 * if (adjacentComp == null || adjacentSide == SwingConstants.LEFT || adjacentSide == SwingConstants.TOP)
 *	  return null;
 *
 * boolean isHor = (adjacentSide == SwingConstants.LEFT || adjacentSide == SwingConstants.RIGHT);
 *
 * if (adjacentComp.getComponetType(false) == ComponentWrapper.TYPE_LABEL && comp.getComponetType(false) == ComponentWrapper.TYPE_TEXT_FIELD)
 *    return isHor ? UNRELATED_Y : UNRELATED_Y;
 *
 * return (adjacentSide == SwingConstants.LEFT || adjacentSide == SwingConstants.RIGHT) ? RELATED_X : RELATED_Y;
 * </pre
 */
public interface InCellGapProvider
{
	/** Returns the default gap between two components that <b>are in the same cell</b>.
	 * @param comp The component that the gap is for. Never <code>null</code>.
	 * @param adjacentComp The adjacent component if any. May be <code>null</code>.
	 * @param adjacentSide What side the <code>adjacentComp</code> is on. {@link javax.swing.SwingUtilities#TOP} or
	 * {@link javax.swing.SwingUtilities#LEFT} or {@link javax.swing.SwingUtilities#BOTTOM} or {@link javax.swing.SwingUtilities#RIGHT}.
	 * @param tag The tag string that the component might be tagged with in the component constraints. May be <code>null</code>.
	 * @param isLTR If it is left-to-right.
	 * @return The default gap between two components or <code>null</code> if there should be no gap.
	 */
	public abstract BoundSize getDefaultGap(ComponentWrapper comp, ComponentWrapper adjacentComp, int adjacentSide, String tag, boolean isLTR);
}
