import apiData from '$lib/data/api.json';

export const prerender = true;

const categoryMeta: Record<string, { title: string; description: string; guide?: string }> = {
	language: {
		title: 'Language',
		description: 'General-purpose modules — collections, strings, math, JSON, and more.'
	},
	web: {
		title: 'Web Framework',
		description: 'Routing, middleware, request handling, and structured error responses.',
		guide: '/guides/web'
	},
	database: {
		title: 'Database',
		description: 'SQLite, Postgres, and MySQL with parameterized queries.',
		guide: '/guides/database'
	}
};

export function load() {
	const categories = Object.entries(categoryMeta).map(([slug, meta]) => {
		const modules = apiData.modules.filter((m: any) => m.category === slug);
		const fnCount = modules.reduce(
			(sum: number, m: any) => sum + m.functions.length,
			0
		);
		return {
			slug,
			title: meta.title,
			description: meta.description,
			guide: meta.guide ?? null,
			moduleCount: modules.length,
			fnCount,
			moduleNames: modules.map((m: any) => m.name)
		};
	});

	return { categories, modules: apiData.modules };
}
