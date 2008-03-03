package net.miginfocom.examples;

import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;

public class Example02
{
	private static JPanel createPanel()
	{
		JPanel panel = new JPanel(new MigLayout());

		panel.add(createLabel("West Panel"),    "dock west");
		panel.add(createLabel("North 1 Panel"), "dock north");
		panel.add(createLabel("North 2 Panel"), "dock north");
		panel.add(createLabel("South Panel"),   "dock south");
		panel.add(createLabel("East Panel"),    "dock east");
		panel.add(createLabel("Center Panel"),  "grow, push"); // "dock center" from v3.0

		return panel;
	}

	private static JLabel createLabel(String text)
	{
		JLabel label = new JLabel(text);
		label.setHorizontalAlignment(JLabel.CENTER);
		label.setBorder(new CompoundBorder(new EtchedBorder(), new EmptyBorder(5, 10, 5, 10)));
		return label;
	}

	public static void main(String[] args)
	{
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				try {
					UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
				} catch (Exception ex) {
					ex.printStackTrace();
				}

				JFrame frame = new JFrame("Example 02");
				frame.getContentPane().add(createPanel());
				frame.pack();
				frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
				frame.setVisible(true);
			}
		});
	}

}
