import apiData from '$lib/data/api.json';

export const prerender = true;

const categoryMeta: Record<string, { title: string; description: string; guide?: string }> = {
	language: {
		title: 'Language',
		description: 'General-purpose modules — collections, strings, math, JSON, and more.'
	},
	server: {
		title: 'Server',
		description:
			'Web framework, database access, authentication, and real-time communication.',
		guide: '/guides/web'
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
