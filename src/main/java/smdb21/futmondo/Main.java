package smdb21.futmondo;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Set;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import gnu.trove.set.hash.THashSet;
import smdb21.futmondo.model.Compra;
import smdb21.futmondo.model.TipoCompra;

public class Main {
	public final static SimpleDateFormat formatDate = new SimpleDateFormat("dd/MM/yyyy - HH:mm:ss");
	public final static SimpleDateFormat formatDateNoSeconds = new SimpleDateFormat("dd/MM/yyyy - HH:mm");

	public static void main(String[] args) {
		final File folder = new File(args[0]);

		try {
			final Set<Compra> compras = readComprasFromFile(folder);
			int originalSize = compras.size();
			compras.addAll(readVestuario(folder));
			System.out.println((compras.size() - originalSize) + " nuevas compras en el vestuario");

			originalSize = compras.size();
			compras.addAll(readSaladeprensa(folder));
			System.out.println((compras.size() - originalSize) + " nuevas compras en la sala de prensa");

			System.out.println(compras.size() + " compras");

			exportCompras(folder, compras);
			System.out.println("Everything OK");
		} catch (final IOException | ParseException e) {
			e.printStackTrace();
			System.exit(-1);
		}
	}

	private static File getTextFile(File folder) {
		return new File(folder.getAbsolutePath() + File.separator + "webapp\\futmondo\\www\\compras.txt");
	}

	private static Set<Compra> readComprasFromFile(File folder) throws IOException, ParseException {
		final Set<Compra> ret = new THashSet<Compra>();

		final File file = getTextFile(folder);
		try {
			if (!file.exists()) {
				return ret;
			}
			final List<String> lines = Files.readAllLines(file.toPath(), StandardCharsets.UTF_8);
			int num = 0;
			for (final String string : lines) {
				num++;
				if (num == 1) {
					continue;
				}
				final Compra compra = Compra.fromLine(string);
				ret.add(compra);
			}

			return ret;
		} finally {
			System.out.println(ret.size() + " compras leidas de fichero " + file.getAbsolutePath());
		}
	}

	private static void exportCompras(File folder, Set<Compra> compras) throws IOException {
		final File file = getTextFile(folder);
		final OutputStreamWriter w = new OutputStreamWriter(new FileOutputStream(file), StandardCharsets.UTF_8);
		w.write(Compra.headerString() + "\n");
		for (final Compra compra : compras) {
			w.write(compra.toString() + "\n");
		}
		w.close();

	}

	private static Set<Compra> readSaladeprensa(File folder) throws IOException, ParseException {
		final Set<Compra> ret = new THashSet<Compra>();
		final File[] files = folder.listFiles(new FilenameFilter() {

			@Override
			public boolean accept(File dir, String name) {
				if (name.startsWith("saladeprensa")) {
					return true;
				}
				return false;
			}
		});

		for (final File file : files) {

			final Document doc = Jsoup.parse(file, "UTF-8");
			final Elements players = doc.getElementsByClass("player");
			for (final Element player : players) {
				final Element img = player.getElementsByTag("img").get(0);
				final String photoURL = img.attr("src").trim();
				final Element li = player.getElementsByClass("text").get(0);
				final String jugador = li.getElementsByTag("strong").get(0).text().trim();
				final Date fecha = formatDateNoSeconds.parse(li.getElementsByTag("time").get(0).text());
				final int precio = parsePrecio(li.getElementsByTag("span").get(0).text().trim());
				final Element from = li.getElementsByClass("from").get(0);
				final Elements strongs = from.getElementsByTag("strong");
				final String vendedor = strongs.get(0).text().trim();
				final String comprador = strongs.get(1).text().trim();
				final TipoCompra tipoCompra = TipoCompra.COMPRA;
				final Compra compra = new Compra(comprador, vendedor, jugador, precio, tipoCompra, fecha, photoURL);
				ret.add(compra);
			}
		}
		return ret;
	}

	private static Set<Compra> readVestuario(File folder) throws IOException, ParseException {
		final Set<Compra> ret = new THashSet<Compra>();
		final File[] files = folder.listFiles(new FilenameFilter() {

			@Override
			public boolean accept(File dir, String name) {
				if (name.startsWith("vestuario")) {
					return true;
				}
				return false;
			}
		});

		for (final File file : files) {

			final Document doc = Jsoup.parse(file, "UTF-8");
			final Elements elements = doc.getElementsByTag("article");
			/*
			 * <article> <a class="user">Miguel</a> <time>14/09/2020 - 05:44:31</time> <p/>
			 * <p>El equipo <strong>Pineda team</strong> ha pagado
			 * <strong>13.063.117</strong> propiedad de <strong>Hala Modric</strong> como
			 * clausula de <strong>Mikel Rico</strong> </p> <p/> </article>
			 * 
			 * 
			 */
			for (final Element article : elements) {
				final String comprador = article.getElementsByTag("a").get(0).text();

				final Elements ps = article.getElementsByTag("p");
				for (final Element p : ps) {
					if (p.text().startsWith("El equipo")) {
						final Date fecha = formatDate.parse(article.getElementsByTag("time").get(0).text());
						final Elements strongs = p.getElementsByTag("strong");
						final String equipoComprador = strongs.get(0).text().trim();
						final int precio = parsePrecio(strongs.get(1).text().trim());
						final String equipoVendedor = strongs.get(2).text().trim();
						final String jugador = strongs.get(3).text().trim();
						final TipoCompra tipo = TipoCompra.CLAUSULA;
						final Compra compra = new Compra(equipoComprador, equipoVendedor, jugador, precio, tipo, fecha,
								null);
						ret.add(compra);
					}
				}
			}

		}

		return ret;
	}

	private static int parsePrecio(String text) {
		if (text == null) {
			return 0;
		}
		return Integer.valueOf(text.replace(".", "").replace(",", "").replace("$", "").replace("€", "").trim());
	}

}
