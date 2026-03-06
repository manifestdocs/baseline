import apiData from '$lib/data/api.json';
import { error } from '@sveltejs/kit';

export const prerender = true;

const categoryMeta: Record<string, { title: string; description: string; guide?: string }> = {
    language: {
        title: 'Language',
        description:
            'General-purpose modules available across all prelude levels — collections, strings, math, JSON, and more.'
    },
    server: {
        title: 'Server',
        description:
            'Web framework, database access, authentication, and real-time communication.',
        guide: '/guides/web'
    }
};

export function entries() {
    return Object.keys(categoryMeta).map((slug) => ({ slug }));
}

export function load({ params }: { params: { slug: string } }) {
    const meta = categoryMeta[params.slug];
    if (!meta) {
        error(404, 'Package not found');
    }

    const modules = apiData.modules.filter(
        (m: any) => m.category === params.slug
    );

    return {
        slug: params.slug,
        title: meta.title,
        description: meta.description,
        guide: meta.guide ?? null,
        modules
    };
}
