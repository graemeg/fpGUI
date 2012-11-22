package net.miginfocom.examples;

import net.miginfocom.swing.MigLayout;

import javax.swing.*;

/**
 */
public class Example01
{
	private static JPanel createPanel()
	{
		JPanel panel = new JPanel(new MigLayout());

		panel.add(new JLabel("First Name"));
		panel.add(new JTextField(15));
		panel.add(new JLabel("Surname"),  "gap unrelated");  // Unrelated size is resolved per platform
		panel.add(new JTextField(15),     "wrap");           // Wraps to the next row
		panel.add(new JLabel("Address"));
		panel.add(new JTextField(),       "span, growx");    // Spans cells in row and grows to fit that

		return panel;
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

				JFrame frame = new JFrame("Example 01");
				frame.getContentPane().add(createPanel());
				frame.pack();
				frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
				frame.setVisible(true);
			}
		});
	}
}
